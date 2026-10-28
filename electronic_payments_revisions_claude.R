setwd("C:\\Users\\alessandro.piovani\\Desktop\\github_repo\\NTTS_high_freq_daily")

source("electronic_payments_claude.R")

################################################################################
#                                                                              #
#   FATTURAZIONE ELETTRONICA - REVISIONI: CALENDARIO BASE VS FINALE            #
#   (coefficienti fissi)                                                       #
#                                                                              #
################################################################################
# Da lanciare IN CODA allo script finale della fatturazione (servono data, log,
# cal_all, cal_reg e generate_eom_regressors di utilities.R) dopo
# source("revision.R").
#
# Due versioni, stessa procedura (calendario nel pre-trattamento, coefficienti
# fissi, poi AMB sequenziale 7 -> 365.2425, modello in log):
#   A  calendario base:   12 festività attive in tutti i giorni (type = "All")
#                         + EoM, EoM-1, EoM-2
#   B  calendario finale: i 32 regressori di cal_reg
# Solo coefficienti fissi: con i coefficienti variabili la varianza stimata
# era zero (coefficienti di fatto costanti).
#
# Revisioni: come per l'elettricità, le misure di default di
# calculate_revisions_by_lags() (lags 7, 30, 90, 181, 365, 730; campione
# comune 1:(T - 730)) più le revisioni sull'ultimo anno di ogni vintage, per
# tre gruppi di giorni (ogni giorno in un solo gruppo, nell'ordine):
#   - festività: le 12 festività, in qualsiasi giorno della settimana;
#   - giorni del mese: EoM ... EoM-5, giorni 1, 15 e 20, venerdì lavorativo
#     prima di un fine mese nel weekend;
#   - altri giorni.
# In più: configurazione completa degli outlier per stima e tabella di
# stabilità; per ogni vintage la data della revisione massima di un singolo
# giorno (per capire se viene da un outlier che compare o sparisce).
#
# I risultati di ogni versione sono salvati in rev_dir e riletti se già
# presenti (cancellare il .rds per rifare una versione).
################################################################################

library(R6)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

stopifnot(exists("data"), exists("log"), exists("cal_all"), exists("cal_reg"),
          exists("AMBEstimator"), exists("generate_eom_regressors"))

rev_dir <- "revisions_einvoicing"
dir.create(rev_dir, showWarnings = FALSE)
lags_rev   <- c(7, 30, 90, 181, 365, 730)
recent_win <- 365

# ------------------------------------------------------------------------------
# 1) Calendari e gruppi di giorni
# ------------------------------------------------------------------------------
d_rev <- data[, c("Date", "Value")]
n_rev <- nrow(d_rev)

hol_all <- as.matrix(cal_all)                                   # 12 festività, tutti i giorni
eom_b   <- as.matrix(generate_eom_regressors(d_rev$Date, num_days_before = 2))
cal_basic <- cbind(hol_all, eom_b)
colnames(cal_basic) <- c(paste0("holiday_", seq_len(ncol(hol_all))),
                         paste0("eom_minus_", seq_len(ncol(eom_b)) - 1))
cal_final <- as.matrix(cal_reg)
stopifnot(nrow(cal_basic) == n_rev, nrow(cal_final) == n_rev,
          qr(cal_basic)$rank == ncol(cal_basic), qr(cal_final)$rank == ncol(cal_final))
cat("Basic calendar:", ncol(cal_basic), "regressors; final calendar:", ncol(cal_final), "\n")

wd_r     <- wday(d_rev$Date, week_start = 1)
to_eom_r <- as.integer(days_in_month(d_rev$Date)) - day(d_rev$Date)
is_hol   <- rowSums(hol_all != 0) > 0
is_month <- to_eom_r <= 5 | day(d_rev$Date) %in% c(1, 15, 20) | (wd_r == 5 & to_eom_r %in% 1:2)
day_group <- factor(ifelse(is_hol, "Holidays", ifelse(is_month, "Month days", "Other days")),
                    levels = c("Holidays", "Month days", "Other days"))
print(table(day_group))

# ------------------------------------------------------------------------------
# 2) Stimatore: calendario fisso nel pre-trattamento, poi AMB sequenziale
# ------------------------------------------------------------------------------
AMBFixedCalEstimator <- R6::R6Class("AMBFixedCalEstimator",
                                    inherit = AMBEstimator,
                                    public = list(
                                      run = function() {
                                        x_cut <- adjust_x_pre_pro(self$x_pre_pro, self$data$Value)
                                        pre_pro <- rjd3highfreq::fractionalAirlineEstimation(
                                          y = self$data$Value, x = x_cut, criticalValue = self$criticalValue_pre_pro,
                                          periods = self$periods_pre_pro, outliers = self$outliers_pre_pro,
                                          log = self$log_pre_pro, y_time = self$data$Date)
                                        self$last_pre_pro <- pre_pro
                                        
                                        dow <- rjd3highfreq::fractionalAirlineDecomposition(
                                          y = pre_pro$model$linearized, period = self$period_s1, log = FALSE, y_time = self$data$Date)
                                        doy <- rjd3highfreq::fractionalAirlineDecomposition(
                                          y = dow$decomposition$sa, period = self$period_s2, log = FALSE, y_time = self$data$Date)
                                        
                                        sa <- doy$decomposition$sa
                                        ls <- pre_pro$model$component_ls
                                        if (!is.null(ls) && length(ls) == length(sa) && !all(is.na(ls)))
                                          sa <- if (self$log_pre_pro) sa + log(ls) else sa + ls
                                        if (self$log_pre_pro) sa <- exp(sa)
                                        as.numeric(sa)
                                      }
                                    )
)

make_estimator <- function(cal) {
  AMBFixedCalEstimator$new(data = d_rev, x_pre_pro = cal, cal_reg_pre_pro = NULL, log_pre_pro = log)
}

# ------------------------------------------------------------------------------
# 3) Revisioni: default di revision.R + ultimo anno per gruppo + outlier
# ------------------------------------------------------------------------------
outlier_names <- function(pre_pro) {
  v <- as.character(pre_pro$model$variables)
  grep("\\d{4}-\\d{2}-\\d{2}", v, value = TRUE)        # solo outlier (nomi con data)
}
name_dates <- function(v) as.Date(regmatches(v, regexpr("\\d{4}-\\d{2}-\\d{2}", v)))

revisions_by_lags_cal <- function(estimator_instance, lags = lags_rev,
                                  group = day_group, window = recent_win) {
  T <- nrow(estimator_instance$data)
  dates <- estimator_instance$data$Date
  
  S_final <- estimator_instance$run()
  stopifnot(length(S_final) == T)
  vars_by_lag <- list(full_sample = outlier_names(estimator_instance$last_pre_pro))
  
  default <- list(); recent <- list(); vint <- list()
  common_t <- 1:(T - max(lags))                    # come in revision.R
  
  for (L in lags) {
    if (L >= T) next
    vintage_end <- T - L
    ev <- estimator_instance$clone(deep = TRUE)
    ev$data      <- estimator_instance$data[1:vintage_end, ]
    ev$x_pre_pro <- ev$x_pre_pro[1:vintage_end, , drop = FALSE]
    S_v <- ev$run()
    vint[[paste0("lag_", L)]] <- S_v
    vars_by_lag[[paste0("lag_", L)]] <- outlier_names(ev$last_pre_pro)
    
    # (a) misure di default sul campione comune, più la data della revisione massima
    r <- S_final[common_t] - S_v[common_t]
    default[[length(default) + 1]] <- data.frame(
      lag_values = L, MR = mean(r), MAR = mean(abs(r)),
      RMAR = mean(abs(r / S_final[common_t])) * 100, SD = sd(r),
      RMSR = sqrt(mean(r^2)), MinRev = min(r), MaxRev = max(r),
      MaxAbsRev_pct  = max(abs(r / S_final[common_t])) * 100,
      MaxAbsRev_date = dates[common_t][which.max(abs(r / S_final[common_t]))])
    
    # (b) ultimo anno del vintage, per gruppo di giorni
    idx <- max(1, vintage_end - window + 1):vintage_end
    rr  <- abs(S_final[idx] - S_v[idx]) / S_final[idx] * 100
    g   <- group[idx]
    recent[[length(recent) + 1]] <- data.frame(
      lag_values = L, vintage_end = dates[vintage_end], RMAR_all = mean(rr),
      RMAR_holidays = mean(rr[g == "Holidays"]), RMAR_month = mean(rr[g == "Month days"]),
      RMAR_other = mean(rr[g == "Other days"]),
      n_holidays = sum(g == "Holidays"), n_month = sum(g == "Month days"))
    cat("  lag", L, "done\n")
  }
  
  list(default = bind_rows(default), recent = bind_rows(recent),
       variables_by_lag = vars_by_lag, S_final = S_final, vintages = vint)
}

# ------------------------------------------------------------------------------
# 4) Le due versioni (risultati salvati e riletti se già presenti)
# ------------------------------------------------------------------------------
versions <- list(
  A = list(label = "A: basic, fixed", cal = cal_basic),
  B = list(label = "B: final, fixed", cal = cal_final))

rev_res <- list()
for (v in names(versions)) {
  f <- file.path(rev_dir, paste0("revisions_", v, ".rds"))
  if (file.exists(f)) {
    cat("Version", v, ": read from", f, "\n")
    rev_res[[v]] <- readRDS(f)
  } else {
    cat("Version", v, ":", versions[[v]]$label, "\n")
    t0 <- Sys.time()
    rev_res[[v]] <- revisions_by_lags_cal(make_estimator(versions[[v]]$cal))
    saveRDS(rev_res[[v]], f)
    cat("  time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
  }
}

lab_of  <- function(v) versions[[v]]$label
collect <- function(part) bind_rows(lapply(names(rev_res), function(v)
  rev_res[[v]][[part]] %>% mutate(version = lab_of(v), .before = 1)))

# ------------------------------------------------------------------------------
# 5) Tabelle
# ------------------------------------------------------------------------------
tab_default <- collect("default")
tab_recent  <- collect("recent")

cat("\n=== Default revisions (common sample up to T - 730), RMAR % ===\n")
print(tab_default %>% select(version, lag_values, RMAR) %>% mutate(RMAR = round(RMAR, 3)) %>%
        pivot_wider(names_from = lag_values, values_from = RMAR, names_prefix = "lag_") %>%
        as.data.frame())

cat("\n=== Default revisions, full table (with the largest single-day revision) ===\n")
print(tab_default %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())

cat("\n=== Revisions over the last", recent_win, "days of each vintage, RMAR % ===\n")
print(tab_recent %>% mutate(across(starts_with("RMAR"), ~ round(.x, 3))) %>% as.data.frame())

tab_summary <- tab_default %>% group_by(version) %>% summarise(RMAR_default = mean(RMAR)) %>%
  left_join(tab_recent %>% group_by(version) %>%
              summarise(RMAR_recent_all = mean(RMAR_all), RMAR_recent_holidays = mean(RMAR_holidays),
                        RMAR_recent_month = mean(RMAR_month), RMAR_recent_other = mean(RMAR_other)),
            by = "version") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\n=== Mean over lags ===\n")
print(as.data.frame(tab_summary))

# Outlier: configurazione completa e stabilità tra le stime
outlier_stability <- function(v) {
  vb  <- rev_res[[v]]$variables_by_lag
  all <- sort(unique(unlist(vb)))
  if (length(all) == 0) return(NULL)
  m <- matrix(sapply(vb, function(x) all %in% x), nrow = length(all), dimnames = list(all, names(vb)))
  od <- name_dates(all)
  data.frame(version = lab_of(v), outlier = all,
             day_group = as.character(day_group[match(od, d_rev$Date)]),
             weekday = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[wday(od, week_start = 1)],
             n_runs = rowSums(m), ifelse(m, "x", ""), check.names = FALSE, row.names = NULL)
}
tab_outl_stab <- bind_rows(lapply(names(rev_res), outlier_stability))
cat("\n=== Outlier configurations: presence in the full sample and in each vintage ===\n")
cat("(an outlier dated after the end of a vintage cannot appear in it)\n")
print(tab_outl_stab, row.names = FALSE)
write.csv(tab_outl_stab, file.path(rev_dir, "outlier_configurations.csv"), row.names = FALSE)

cat("\n=== Outliers in the full sample, by group of days ===\n")
print(tab_outl_stab %>% dplyr::filter(full_sample == "x") %>% count(version, day_group) %>% as.data.frame())

# ------------------------------------------------------------------------------
# 6) Figure (PNG + PDF), stesso stile dell'elettricità
# ------------------------------------------------------------------------------
ver_cols <- c("A: basic, fixed" = "#E69F00", "B: final, fixed" = "#56B4E9")

save_rev <- function(p, name, w = 16, h = 9) {
  ggsave(file.path(rev_dir, paste0(name, ".png")), p, width = w, height = h, units = "cm", dpi = 300)
  ggsave(file.path(rev_dir, paste0(name, ".pdf")), p, width = w, height = h, units = "cm")
  cat("Saved:", file.path(rev_dir, paste0(name, c(".png", ".pdf"))), "\n")
}
theme_rev <- theme_classic(base_size = 11) +
  theme(legend.position = "top", legend.title = element_blank(),
        strip.background = element_blank(), strip.text = element_text(face = "bold"))

p <- ggplot(tab_default, aes(lag_values, RMAR, colour = version)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.8) +
  scale_x_continuous(breaks = lags_rev, trans = "log10") +
  scale_colour_manual(values = ver_cols) +
  labs(x = "Lag (days, log scale)", y = "Mean absolute revision (%)") + theme_rev
save_rev(p, "revisions_default_by_lag")

rec_long <- tab_recent %>%
  select(version, lag_values, RMAR_holidays, RMAR_month, RMAR_other) %>%
  pivot_longer(starts_with("RMAR"), names_to = "days", values_to = "RMAR") %>%
  mutate(days = factor(days, c("RMAR_holidays", "RMAR_month", "RMAR_other"),
                       c("Holidays", "Month days", "Other days")))
p <- ggplot(rec_long, aes(lag_values, RMAR, colour = version)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.8) +
  facet_wrap(~ days, nrow = 1) +
  scale_x_continuous(breaks = lags_rev, trans = "log10") +
  scale_colour_manual(values = ver_cols) +
  labs(x = "Lag (days, log scale)", y = "Mean absolute revision, last year of vintage (%)") + theme_rev
save_rev(p, "revisions_recent_by_group", w = 18, h = 9)

save.image(file = file.path(rev_dir, "workspace_revisions_einvoicing.RData"))