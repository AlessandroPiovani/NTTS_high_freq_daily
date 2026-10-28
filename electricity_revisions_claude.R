setwd("C:\\Users\\alessandro.piovani\\Desktop\\github_repo\\NTTS_high_freq_daily")

source("electricity_demand_IT_claude.R")

################################################################################
#                                                                              #
#   DOMANDA ELETTRICA - REVISIONI: CALENDARIO BASE VS FINALE,                  #
#   COEFFICIENTI FISSI VS VARIABILI                                            #
#                                                                              #
################################################################################
# Da lanciare IN CODA allo script finale dell'elettricità (servono data, log,
# cal_all e cal_reg del PASSO 4) dopo source("revision.R").
#
# Quattro versioni (2 calendari x 2 stime), stessa decomposizione AMB
# sequenziale (7, poi 365.2425):
#
#                                 coefficienti fissi     coefficienti variabili
#   calendario base (12 fest.)          A                        C
#   calendario finale (23 regr.)        B                        D  <- scelta attuale
#
#   - calendario base: 12 festività attive in tutti i giorni (type = "All");
#   - coefficienti fissi: calendario nel pre-trattamento (x di
#     fractionalAirlineEstimation), stimato insieme a ARIMA e outlier;
#   - coefficienti variabili: calendario stimato prima con rjd3sts (airline
#     settimanale + regressori a passeggiata casuale) e tolto dalla serie,
#     poi pre-trattamento solo per gli outlier (come pre_processing() di
#     revision.R).
#   A vs B e C vs D: effetto del contenuto del calendario.
#   A vs C e B vs D: effetto dei coefficienti variabili.
#
# Revisioni: le stesse di default di calculate_revisions_by_lags()
# (lags = 7, 30, 90, 181, 365, 730; stesse misure MR, MAR, RMAR, SD, RMSR,
# Min, Max; stesso campione comune 1:(T - 730)). La funzione qui sotto è una
# copia di quella di revision.R che in più:
#   1. tiene le serie di ogni vintage, così ogni stima si fa una volta sola;
#   2. calcola anche le revisioni sull'ULTIMO ANNO di ogni vintage, separando
#      giorni speciali (festività, ponti, semi-chiusure, Pasqua) e giorni
#      normali. Il campione comune di default (1:(T-730)) misura la stabilità
#      della storia fino a fine 2023; il calendario conta soprattutto alla fine
#      del campione, dove i coefficienti variabili sono solo filtrati e gli
#      errori di specificazione diventano outlier;
#   3. conta gli outlier che in ogni vintage cadono su giorni speciali.
#
# Costo: 7 stime per versione (campione intero + 6 vintage), 28 in tutto; le
# versioni C e D (rjd3sts) sono le più lente. I risultati di ogni versione
# sono salvati in rev_dir e, se ci sono già, vengono riletti invece di
# ristimare (basta cancellare il file .rds per rifare una versione).
################################################################################

library(R6)
library(dplyr)
library(tidyr)
library(ggplot2)

stopifnot(exists("data"), exists("log"), exists("cal_all"), exists("cal_reg"),
          exists("AMBEstimator"))   # AMBEstimator viene da revision.R

rev_dir <- "revisions_electricity"
dir.create(rev_dir, showWarnings = FALSE)
lags_rev   <- c(7, 30, 90, 181, 365, 730)    # default di calculate_revisions_by_lags
recent_win <- 365                             # ultimo anno di ogni vintage

# ------------------------------------------------------------------------------
# 1) Calendari e giorni speciali
# ------------------------------------------------------------------------------
d_rev <- data[, c("Date", "Value")]

cal_basic <- as.matrix(cal_all)                              # 12 festività, tutti i giorni
colnames(cal_basic) <- c("Liberation Day", "Republic Day", "Immaculate Conception", "St. Stephen's Day",
                         "Epiphany", "New Year's Day", "Easter Monday", "Labour Day",
                         "Assumption of Mary", "All Saints' Day", "Christmas", "Easter Sunday")
cal_final <- as.matrix(cal_reg)                              # 23 regressori del PASSO 4
stopifnot(nrow(cal_basic) == nrow(d_rev), nrow(cal_final) == nrow(d_rev), ncol(cal_final) == 23)

# Giorno speciale = attivo in un regressore del calendario finale, oppure
# festività in qualsiasi giorno della settimana (per includere le festività
# del weekend, che il calendario finale tratta come weekend normali)
special_day <- rowSums(cal_final != 0) > 0 | rowSums(cal_basic[, 1:11] != 0) > 0
cat("Special days:", sum(special_day), "of", length(special_day), "\n")

# ------------------------------------------------------------------------------
# 2) Pre-trattamento con calendario fisso o variabile
# ------------------------------------------------------------------------------
# time_varying = TRUE : come pre_processing() di revision.R (rjd3sts, poi
#                       fractionalAirlineEstimation sulla serie corretta)
# time_varying = FALSE: niente rjd3sts; il calendario entra come x_pre_pro
pre_processing_cal <- function(data, cal_reg, time_varying, x_pre_pro, criticalValue,
                               periods, log, outliers) {
  if (time_varying) {
    stopifnot(is.matrix(cal_reg), nrow(cal_reg) == nrow(data))
    sm <- model()
    add(sm, sarima("airline", 7, c(0, 1, 1), c(0, 1, 1)))
    add(sm, reg("cal", cal_reg, 0.1))
    y_sts <- if (log) log(data$Value) else data$Value
    r <- rjd3sts::estimate(sm, y_sts, marginal = F, initialization = "SqrtDiffuse",
                           optimizer = "LevenbergMarquardt", concentrated = T, precision = 1e-10)
    cal_eff <- result(r, "ssf.smoothing.components")[, 2]
    y_adj   <- if (log) data$Value / exp(cal_eff) else data$Value - cal_eff
  } else {
    stopifnot(is.matrix(x_pre_pro), nrow(x_pre_pro) == nrow(data))
    y_adj <- data$Value
  }
  rjd3highfreq::fractionalAirlineEstimation(
    y = y_adj, x = x_pre_pro, criticalValue = criticalValue, periods = periods,
    outliers = outliers, log = log, y_time = data$Date)
}

# AMBEstimator di revision.R con l'interruttore time_varying; stessa
# decomposizione. Restituisce la serie destagionalizzata e corretta per il
# calendario (SCA), LS rimessi.
AMBCalEstimator <- R6::R6Class("AMBCalEstimator",
                               inherit = AMBEstimator,
                               public = list(
                                 time_varying = TRUE,
                                 run = function() {
                                   x_cut <- if (is.null(self$x_pre_pro)) NULL else adjust_x_pre_pro(self$x_pre_pro, self$data$Value)
                                   pre_pro <- pre_processing_cal(
                                     data = self$data, cal_reg = self$cal_reg_pre_pro, time_varying = self$time_varying,
                                     x_pre_pro = x_cut, criticalValue = self$criticalValue_pre_pro,
                                     periods = self$periods_pre_pro, log = self$log_pre_pro, outliers = self$outliers_pre_pro)
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

make_estimator <- function(cal, time_varying) {
  e <- AMBCalEstimator$new(
    data            = d_rev,
    x_pre_pro       = if (time_varying) NULL else cal,
    cal_reg_pre_pro = if (time_varying) cal else NULL,
    log_pre_pro     = log)
  e$time_varying <- time_varying
  e
}

# ------------------------------------------------------------------------------
# 3) Revisioni: default di revision.R + ultimo anno + outlier su giorni speciali
# ------------------------------------------------------------------------------
outlier_dates <- function(pre_pro) {
  v <- as.character(pre_pro$model$variables)
  as.Date(regmatches(v, regexpr("\\d{4}-\\d{2}-\\d{2}", v)))
}

revisions_by_lags_cal <- function(estimator_instance, lags = lags_rev,
                                  special = special_day, window = recent_win) {
  T <- nrow(estimator_instance$data)
  dates <- estimator_instance$data$Date
  
  S_final <- estimator_instance$run()
  stopifnot(length(S_final) == T)
  out_final <- outlier_dates(estimator_instance$last_pre_pro)
  vars_final <- as.character(estimator_instance$last_pre_pro$model$variables)
  vars_by_lag <- list(full_sample = vars_final)   # configurazione completa degli outlier
  
  default <- list(); recent <- list(); outl <- list(); vint <- list()
  common_t <- 1:(T - max(lags))                    # come in revision.R
  
  for (L in lags) {
    if (L >= T) next
    vintage_end <- T - L
    ev <- estimator_instance$clone(deep = TRUE)
    ev$data <- estimator_instance$data[1:vintage_end, ]
    if (!is.null(ev$cal_reg_pre_pro)) ev$cal_reg_pre_pro <- ev$cal_reg_pre_pro[1:vintage_end, , drop = FALSE]
    if (!is.null(ev$x_pre_pro))       ev$x_pre_pro       <- ev$x_pre_pro[1:vintage_end, , drop = FALSE]
    S_v <- ev$run()
    vint[[paste0("lag_", L)]] <- S_v
    
    # (a) misure di default, campione comune 1:(T - max(lags))
    r <- S_final[common_t] - S_v[common_t]
    default[[length(default) + 1]] <- data.frame(
      lag_values = L, MR = mean(r), MAR = mean(abs(r)),
      RMAR = mean(abs(r / S_final[common_t])) * 100, SD = sd(r),
      RMSR = sqrt(mean(r^2)), MinRev = min(r), MaxRev = max(r))
    
    # (b) ultimo anno del vintage, giorni speciali e normali
    idx <- max(1, vintage_end - window + 1):vintage_end
    rr  <- abs(S_final[idx] - S_v[idx]) / S_final[idx] * 100
    sp  <- special[idx]
    recent[[length(recent) + 1]] <- data.frame(
      lag_values = L, vintage_end = dates[vintage_end],
      RMAR_all = mean(rr), RMAR_special = mean(rr[sp]), RMAR_normal = mean(rr[!sp]),
      P90_special = unname(quantile(rr[sp], 0.9)), n_special = sum(sp))
    
    # (c) outlier del vintage: configurazione completa e quanti su giorni speciali
    vars_by_lag[[paste0("lag_", L)]] <- as.character(ev$last_pre_pro$model$variables)
    od <- outlier_dates(ev$last_pre_pro)
    outl[[length(outl) + 1]] <- data.frame(
      lag_values = L, n_outliers = length(od),
      on_special = sum(special[match(od, dates)], na.rm = TRUE),
      dates_on_special = paste(format(od[special[match(od, dates)] %in% TRUE]), collapse = ", "))
    cat("  lag", L, "done\n")
  }
  
  list(default = bind_rows(default), recent = bind_rows(recent), outliers = bind_rows(outl),
       outliers_full = out_final, variables_by_lag = vars_by_lag, S_final = S_final, vintages = vint)
}

# ------------------------------------------------------------------------------
# 4) Le quattro versioni (risultati salvati e riletti se già presenti)
# ------------------------------------------------------------------------------
# Ordine: prima i coefficienti fissi (A, B), poi quelli variabili (C, D);
# D = specificazione adottata (calendario finale, coefficienti variabili).
# I file .rds hanno un nome descrittivo, indipendente dalla lettera.
versions <- list(
  A = list(label = "A: basic, fixed",          cal = cal_basic, tv = FALSE, id = "basic_fixed"),
  B = list(label = "B: final, fixed",          cal = cal_final, tv = FALSE, id = "final_fixed"),
  C = list(label = "C: basic, time-varying",   cal = cal_basic, tv = TRUE,  id = "basic_tv"),
  D = list(label = "D: final, time-varying",   cal = cal_final, tv = TRUE,  id = "final_tv"))

# Migrazione dai vecchi nomi (revisions_A..D.rds, con C = finale variabile e
# D = base variabile): rinomina i file già calcolati, senza ristimare
old_ids <- c(A = "basic_fixed", B = "final_fixed", C = "final_tv", D = "basic_tv")
for (o in names(old_ids)) {
  f_old <- file.path(rev_dir, paste0("revisions_", o, ".rds"))
  f_new <- file.path(rev_dir, paste0("revisions_", old_ids[[o]], ".rds"))
  if (file.exists(f_old) && !file.exists(f_new)) file.rename(f_old, f_new)
}

rev_res <- list()
for (v in names(versions)) {
  f <- file.path(rev_dir, paste0("revisions_", versions[[v]]$id, ".rds"))
  if (file.exists(f)) {
    cat("Version", v, ": read from", f, "\n")
    rev_res[[v]] <- readRDS(f)
  } else {
    cat("Version", v, ":", versions[[v]]$label, "\n")
    t0 <- Sys.time()
    rev_res[[v]] <- revisions_by_lags_cal(make_estimator(versions[[v]]$cal, versions[[v]]$tv))
    saveRDS(rev_res[[v]], f)
    cat("  time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
  }
}

lab_of <- function(v) versions[[v]]$label
collect <- function(part) bind_rows(lapply(names(rev_res), function(v)
  rev_res[[v]][[part]] %>% mutate(version = lab_of(v), .before = 1)))

# ------------------------------------------------------------------------------
# 5) Tabelle
# ------------------------------------------------------------------------------
tab_default <- collect("default")
tab_recent  <- collect("recent")
tab_outl    <- collect("outliers")

cat("\n=== Default revisions (common sample up to T - 730), RMAR % ===\n")
print(tab_default %>% select(version, lag_values, RMAR) %>%
        mutate(RMAR = round(RMAR, 3)) %>%
        pivot_wider(names_from = lag_values, values_from = RMAR, names_prefix = "lag_") %>%
        as.data.frame())

cat("\n=== Default revisions, full table ===\n")
print(tab_default %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame())

cat("\n=== Revisions over the last", recent_win, "days of each vintage, RMAR % ===\n")
print(tab_recent %>% mutate(across(c(RMAR_all, RMAR_special, RMAR_normal, P90_special), ~ round(.x, 3))) %>%
        as.data.frame())

cat("\n=== Mean over lags ===\n")
tab_summary <- tab_default %>% group_by(version) %>% summarise(RMAR_default = mean(RMAR)) %>%
  left_join(tab_recent %>% group_by(version) %>%
              summarise(RMAR_recent_all     = mean(RMAR_all),
                        RMAR_recent_special = mean(RMAR_special),
                        RMAR_recent_normal  = mean(RMAR_normal)), by = "version") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(as.data.frame(tab_summary))

cat("\n=== Outliers in each vintage, and how many fall on special days ===\n")
print(tab_outl %>% as.data.frame())
for (v in names(rev_res)) {
  od <- rev_res[[v]]$outliers_full
  cat(lab_of(v), "- full sample:", length(od), "outliers,",
      sum(special_day[match(od, d_rev$Date)], na.rm = TRUE), "on special days\n")
}

# Stabilità della configurazione degli outlier: per ogni versione, ogni
# outlier (tipo e data) e in quali stime compare. Un outlier presente solo
# nei vintage (e non nel campione intero) è stato poi "assorbito"; uno su un
# giorno speciale che va e viene segnala un calendario specificato male.
outlier_stability <- function(v) {
  # solo gli outlier (nomi con una data, es. "LS.2020-03-15"): nelle versioni
  # a coefficienti fissi model$variables contiene anche i regressori di calendario
  vb  <- lapply(rev_res[[v]]$variables_by_lag, function(x) grep("\\d{4}-\\d{2}-\\d{2}", x, value = TRUE))
  all <- sort(unique(unlist(vb)))
  if (length(all) == 0) return(NULL)
  m <- sapply(vb, function(x) all %in% x)
  m <- matrix(m, nrow = length(all), dimnames = list(all, names(vb)))
  od <- as.Date(regmatches(all, regexpr("\\d{4}-\\d{2}-\\d{2}", all)))
  data.frame(version = lab_of(v), outlier = all,
             special_day = special_day[match(od, d_rev$Date)] %in% TRUE,
             n_runs = rowSums(m), ifelse(m, "x", ""), check.names = FALSE, row.names = NULL)
}
tab_outl_stab <- bind_rows(lapply(names(rev_res), outlier_stability))
cat("\n=== Outlier configurations: presence in the full sample and in each vintage ===\n")
print(tab_outl_stab, row.names = FALSE)
write.csv(tab_outl_stab, file.path(rev_dir, "outlier_configurations.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# 6) Figure (PNG + PDF)
# ------------------------------------------------------------------------------
# stessi colori di prima per ciascuna specificazione (cambia solo la lettera)
ver_cols <- c("A: basic, fixed" = "#E69F00", "B: final, fixed" = "#56B4E9",
              "C: basic, time-varying" = "#D55E00", "D: final, time-varying" = "#0072B2")
ver_lty  <- c("A: basic, fixed" = 2, "B: final, fixed" = 2,
              "C: basic, time-varying" = 1, "D: final, time-varying" = 1)

save_rev <- function(p, name, w = 16, h = 9) {
  ggsave(file.path(rev_dir, paste0(name, ".png")), p, width = w, height = h, units = "cm", dpi = 300)
  ggsave(file.path(rev_dir, paste0(name, ".pdf")), p, width = w, height = h, units = "cm")
  cat("Saved:", file.path(rev_dir, paste0(name, c(".png", ".pdf"))), "\n")
}
theme_rev <- theme_classic(base_size = 11) +
  theme(legend.position = "top", legend.title = element_blank(),
        strip.background = element_blank(), strip.text = element_text(face = "bold"))

# 6a) Revisioni di default per lag
p <- ggplot(tab_default, aes(lag_values, RMAR, colour = version, linetype = version)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.8) +
  scale_x_continuous(breaks = lags_rev, trans = "log10") +
  scale_colour_manual(values = ver_cols) + scale_linetype_manual(values = ver_lty) +
  labs(x = "Lag (days, log scale)", y = "Mean absolute revision (%)") +
  guides(colour = guide_legend(nrow = 2)) + theme_rev
save_rev(p, "revisions_default_by_lag")

# 6b) Ultimo anno del vintage: giorni speciali e normali
rec_long <- tab_recent %>%
  select(version, lag_values, RMAR_special, RMAR_normal) %>%
  pivot_longer(c(RMAR_special, RMAR_normal), names_to = "days", values_to = "RMAR") %>%
  mutate(days = factor(days, c("RMAR_special", "RMAR_normal"), c("Special days", "Normal days")))
p <- ggplot(rec_long, aes(lag_values, RMAR, colour = version, linetype = version)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.8) +
  facet_wrap(~ days, nrow = 1) +
  scale_x_continuous(breaks = lags_rev, trans = "log10") +
  scale_colour_manual(values = ver_cols) + scale_linetype_manual(values = ver_lty) +
  labs(x = "Lag (days, log scale)", y = "Mean absolute revision, last year of vintage (%)") +
  guides(colour = guide_legend(nrow = 2)) + theme_rev +
  # più spazio: etichetta y intera, pannelli staccati ("730" e "7" non si toccano)
  theme(panel.spacing.x = unit(0.8, "cm"), plot.margin = margin(5, 10, 15, 5))
save_rev(p, "revisions_recent_special_vs_normal", h = 11)

save.image(file = file.path(rev_dir, "workspace_revisions_electricity.RData"))