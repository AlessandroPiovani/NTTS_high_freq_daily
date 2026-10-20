################################################################################
#                                                                              #
#   FATTURAZIONE ELETTRONICA - IMMAGINI DEI DMR PER LE SLIDE                   #
#                                                                              #
################################################################################
# Da lanciare DOPO lo script completo della fatturazione (servono data, log,
# italian_calendar, daily e la funzione generate_eom_regressors di utilities.R).
# Non modifica nessun oggetto dello script principale.
#
# Ogni immagine confronta il DMR con il calendario BASE e con quello FINALE:
#   calendario base  = 12 festività (type = "All") + EoM, EoM-1, EoM-2
#   calendario finale = i 32 regressori dello script
#   DMR = SA_CA del giorno / mediana della SA_CA del suo mese (atteso ~1)
#
# Stile comune a tutte le immagini:
#   - punti = mediana del DMR del gruppo di giorni;
#   - arancio = calendario base, blu = calendario finale (colori Okabe-Ito);
#   - fascia grigia = tolleranza ±5%, linea tratteggiata = 1;
#   - nessun titolo nell'immagine: il messaggio va nel titolo della slide;
#   - testo grande, dimensioni pensate per l'area di contenuto di una slide 16:9.
#
# Ogni immagine stampa anche la tabella dei numeri disegnati, per controllo.
################################################################################

library(dplyr)
library(ggplot2)
library(lubridate)

# ------------------------------------------------------------------------------
# 0a) Impostazioni comuni
# ------------------------------------------------------------------------------
out_dir   <- "slides_dmr"                  # cartella delle immagini
fig_w_cm  <- 24                            # larghezza dell'immagine (cm)
fig_h_cm  <- 11                            # altezza dell'immagine (cm)
base_size <- 16                            # corpo del testo (punti)
tol       <- 0.05                          # tolleranza attorno a 1

col_base  <- "#D55E00"                     # calendario base (arancio)
col_final <- "#0072B2"                     # calendario finale (blu)
cal_cols  <- c("Basic calendar" = col_base, "Final calendar" = col_final)

dir.create(out_dir, showWarnings = FALSE)

theme_slides <- function() {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line          = element_line(color = "black"),
          legend.position    = "top",
          legend.title       = element_blank(),
          legend.text        = element_text(size = base_size),
          plot.margin        = margin(5, 10, 5, 5))
}

save_slide <- function(p, name, w = fig_w_cm, h = fig_h_cm) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h, units = "cm", dpi = 300)
  ggsave(file.path(out_dir, paste0(name, ".pdf")), p, width = w, height = h, units = "cm")
  cat("Saved:", file.path(out_dir, paste0(name, c(".png", ".pdf"))), "\n")
}

# Fascia di tolleranza e linea a 1, da aggiungere a ogni grafico
tolerance_layers <- list(
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1 - tol, ymax = 1 + tol,
           fill = "grey88", alpha = 0.8),
  geom_hline(yintercept = 1, linetype = 2, color = "grey40")
)

# ------------------------------------------------------------------------------
# 0b) SA_CA con il calendario BASE (se non è già stata calcolata dall'appendice)
# ------------------------------------------------------------------------------
if (!exists("sa_ca_base")) {
  cal_base <- rjd3toolkit::holidays(calendar = italian_calendar, start = min(data$Date),
                                    length = nrow(data), type = "All", nonworking = c(6, 7))
  eom_base <- generate_eom_regressors(data$Date, num_days_before = 2)   # EoM, EoM-1, EoM-2
  X_base   <- as.matrix(cbind(cal_base, eom_base))
  colnames(X_base) <- c(paste0("holiday_", seq_len(ncol(cal_base))),
                        paste0("eom_minus_", seq_len(ncol(eom_base)) - 1))
  stopifnot(all(colSums(X_base != 0) > 0), qr(X_base)$rank == ncol(X_base))
  
  pre_pro_base <- fractionalAirlineEstimation(
    y = data$Value, x = X_base, log = log, criticalValue = 6,
    periods = c(7, 365.2425), outliers = c("ao", "wo", "ls"), y_time = data$Date)
  dow_base <- rjd3highfreq::fractionalAirlineDecomposition(
    y = pre_pro_base$model$linearized, period = 7, y_time = data$Date)
  doy_base <- rjd3highfreq::fractionalAirlineDecomposition(
    y = dow_base$decomposition$sa, period = 365.2425, y_time = data$Date)
  
  sa_ca_base <- doy_base$decomposition$sa
  ls_base    <- pre_pro_base$model$component_ls
  if (!is.null(ls_base) && length(ls_base) == length(sa_ca_base) && !all(is.na(ls_base))) {
    sa_ca_base <- if (log) sa_ca_base + log(ls_base) else sa_ca_base + ls_base
  }
  if (log) sa_ca_base <- exp(sa_ca_base)
}

# ------------------------------------------------------------------------------
# 0c) Tabella unica dei DMR, calendario base e finale, con tutte le variabili
#     per i raggruppamenti delle slide
# ------------------------------------------------------------------------------
make_dmr <- function(dates, sa_ca, label) {
  data.frame(Date = dates, SA_CA = as.numeric(sa_ca)) %>%
    mutate(month    = floor_date(Date, "month"),
           dom      = day(Date),
           to_eom   = as.integer(days_in_month(Date)) - day(Date),
           wd       = wday(Date, week_start = 1),
           day_type = factor(case_when(wd == 7 ~ "Sunday", wd == 6 ~ "Saturday", TRUE ~ "Weekday"),
                             levels = c("Weekday", "Saturday", "Sunday")),
           calendar = label) %>%
    group_by(month) %>%
    mutate(DMR = SA_CA / median(SA_CA)) %>%
    ungroup()
}

stopifnot(length(sa_ca_base) == nrow(data), all(daily$Date == data$Date))
dmr_all <- bind_rows(make_dmr(data$Date,  sa_ca_base,   "Basic calendar"),
                     make_dmr(daily$Date, daily$SA_CA,  "Final calendar")) %>%
  mutate(calendar = factor(calendar, levels = names(cal_cols)))


################################################################################
#  IMMAGINE 1 - DMR PER GIORNO DEL MESE                                        #
################################################################################
# Messaggio della slide (proposta): "Day of the month: peaks on days 1, 15 and
# 20 and the end-of-month swings disappear with the final calendar"
# ------------------------------------------------------------------------------
img1_data <- dmr_all %>%
  group_by(calendar, dom) %>%
  summarise(median_DMR = median(DMR), n = n(), .groups = "drop")

print(img1_data %>% tidyr::pivot_wider(id_cols = dom, names_from = calendar,
                                       values_from = median_DMR) %>%
        mutate(across(-dom, ~ round(.x, 3))), n = 31)

img1 <- ggplot(img1_data, aes(x = dom, y = median_DMR, color = calendar)) +
  tolerance_layers +
  geom_line(linewidth = 0.6, alpha = 0.6) +
  geom_point(size = 2.6) +
  scale_color_manual(values = cal_cols) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 31), expand = expansion(add = 0.6)) +
  labs(x = "Day of the month", y = "Median DMR") +
  theme_slides()

img1
save_slide(img1, "01_dmr_day_of_month")


################################################################################
#  BOXPLOT DEL DMR PER TUTTI I RAGGRUPPAMENTI (un calendario alla volta)        #
################################################################################
# Una funzione che produce tutte le immagini a boxplot per il calendario scelto:
#   make_dmr_boxplots("Basic calendar", "basic")   -> situazione "prima"
#   make_dmr_boxplots("Final calendar", "final")   -> situazione "dopo"
#
# Immagini (prefisso = "basic" o "final"):
#   B1 giorno della settimana
#   B2 giorno del mese
#   B3 giorni alla fine del mese (EoM ... EoM-9)
#   B4 giorni speciali per tipo di giorno (feriale / sabato / domenica)
#   B5 il giorno della festività, per giorno della settimana della festa
#   B6 attorno alle festività in settimana (da 3 giorni prima a 3 giorni dopo)
#   B7 penultimo e terzultimo giorno del mese (feriali): venerdì e altri giorni
#
# Boxplot standard: scatola = scarto interquartile, linea = mediana, baffi =
# 1,5 x IQR, punti = valori oltre i baffi. L'asse y è tagliato sui baffi:
# i valori estremi entrano nel calcolo delle scatole ma possono non essere
# disegnati (lo stampa a console quando succede).
# ------------------------------------------------------------------------------

# Limiti dell'asse y: dai baffi dei boxplot, sempre inclusa la fascia ±5%
whisker_ylim <- function(df, groups) {
  w <- df %>%
    group_by(across(all_of(groups))) %>%
    summarise(q1 = quantile(DMR, 0.25), q3 = quantile(DMR, 0.75),
              lo = max(min(DMR), q1 - 1.5 * (q3 - q1)),
              hi = min(max(DMR), q3 + 1.5 * (q3 - q1)), .groups = "drop")
  yl <- c(min(w$lo, 1 - tol), max(w$hi, 1 + tol))
  yl + c(-1, 1) * 0.04 * diff(yl)
}

# Boxplot nello stile delle slide
box_dmr <- function(df, xvar, xlab, groups = xvar, facet = NULL, fill_var = NULL,
                    rotate_x = FALSE) {
  yl <- whisker_ylim(df, unique(c(groups, facet, fill_var)))
  n_out <- sum(df$DMR < yl[1] | df$DMR > yl[2])
  if (n_out > 0) cat("Note:", n_out, "extreme values outside the plotted y range\n")
  
  p <- ggplot(df, aes(x = .data[[xvar]], y = DMR)) + tolerance_layers
  if (is.null(fill_var)) {
    p <- p + geom_boxplot(fill = "grey80", color = "black", width = 0.7,
                          outlier.size = 0.7, outlier.alpha = 0.4)
  } else {
    p <- p + geom_boxplot(aes(fill = .data[[fill_var]]), color = "black", width = 0.7,
                          outlier.size = 0.7, outlier.alpha = 0.4,
                          position = position_dodge(0.8)) +
      scale_fill_manual(values = c("grey85", "grey45"))
  }
  if (!is.null(facet)) p <- p + facet_wrap(as.formula(paste("~", facet)), nrow = 1)
  p <- p + coord_cartesian(ylim = yl) +
    labs(x = xlab, y = "DMR") +
    theme_slides() +
    theme(strip.text = element_text(face = "bold", size = base_size))
  if (rotate_x) p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p
}

# Tabella delle mediane (per controllo a console)
print_medians <- function(df, groups) {
  print(as.data.frame(df %>% group_by(across(all_of(groups))) %>%
                        summarise(median_DMR = round(median(DMR), 3), n = n(), .groups = "drop")))
}

make_dmr_boxplots <- function(which_cal, prefix) {
  
  d <- dmr_all %>% dplyr::filter(calendar == which_cal) %>% arrange(Date)
  stopifnot(nrow(d) == nrow(data))
  wd_lab <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  
  # Festività (11, esclusa Pasqua), in qualsiasi giorno della settimana
  hol_all  <- as.matrix(rjd3toolkit::holidays(calendar = italian_calendar, start = min(d$Date),
                                              length = nrow(d), type = "All", nonworking = c(6, 7)))
  is_hol   <- rowSums(hol_all[, 1:11, drop = FALSE]) > 0
  md       <- format(d$Date, "%m-%d")
  xmas_win <- md >= "12-20" | md <= "01-10"
  
  cat("\n=====================", which_cal, "=====================\n")
  
  # B1 - Giorno della settimana ------------------------------------------------
  b1 <- d %>% mutate(x = factor(wd, 1:7, wd_lab))
  cat("\nB1 - Day of the week\n"); print_medians(b1, "x")
  p <- box_dmr(b1, "x", "Day of the week")
  print(p); save_slide(p, paste0(prefix, "_B1_day_of_week"))
  
  # B2 - Giorno del mese ------------------------------------------------------
  b2 <- d %>% mutate(x = factor(dom, 1:31))
  cat("\nB2 - Day of the month\n"); print_medians(b2, "x")
  p <- box_dmr(b2, "x", "Day of the month") +
    scale_x_discrete(breaks = as.character(c(1, 5, 10, 15, 20, 25, 31)))
  print(p); save_slide(p, paste0(prefix, "_B2_day_of_month"))
  
  # B3 - Giorni alla fine del mese --------------------------------------------
  b3 <- d %>% dplyr::filter(to_eom <= 9) %>%
    mutate(x = factor(to_eom, 0:9, c("EoM", paste0("EoM-", 1:9))))
  cat("\nB3 - Days to month end\n"); print_medians(b3, "x")
  p <- box_dmr(b3, "x", "Days to the end of the month")
  print(p); save_slide(p, paste0(prefix, "_B3_days_to_month_end"))
  
  # B4 - Giorni speciali per tipo di giorno -----------------------------------
  b4_levels <- c("EoM", paste0("EoM-", 1:5), "Day 1", "Day 15", "Day 20")
  b4 <- d %>%
    mutate(x = case_when(dom == 1 ~ "Day 1", dom == 15 ~ "Day 15", dom == 20 ~ "Day 20",
                         to_eom == 0 ~ "EoM", to_eom %in% 1:5 ~ paste0("EoM-", to_eom))) %>%
    dplyr::filter(!is.na(x)) %>%
    mutate(x = factor(x, b4_levels))
  cat("\nB4 - Special days by day type\n"); print_medians(b4, c("day_type", "x"))
  p <- box_dmr(b4, "x", NULL, groups = "x", facet = "day_type", rotate_x = TRUE)
  print(p); save_slide(p, paste0(prefix, "_B4_special_days_by_day_type"), h = fig_h_cm + 1)
  
  # B5 - Il giorno della festività, per giorno della settimana della festa -----
  b5 <- d[is_hol, ] %>% mutate(x = factor(wd, 1:7, wd_lab))
  cat("\nB5 - Holiday itself, by weekday of the holiday\n"); print_medians(b5, "x")
  p <- box_dmr(b5, "x", "Weekday of the holiday")
  print(p); save_slide(p, paste0(prefix, "_B5_holiday_by_weekday"))
  
  # B6 - Attorno alle festività in settimana (escluso il periodo natalizio) ----
  h_idx <- which(is_hol & d$wd <= 5 & !xmas_win)
  b6 <- bind_rows(lapply(-3:3, function(k) {
    idx <- h_idx + k
    idx <- idx[idx >= 1 & idx <= nrow(d)]
    d[idx, ] %>% mutate(k = k)
  })) %>%
    mutate(x = factor(k, -3:3, c("H-3", "H-2", "H-1", "Holiday", "H+1", "H+2", "H+3")))
  cat("\nB6 - Around weekday holidays\n"); print_medians(b6, "x")
  p <- box_dmr(b6, "x", "Days around a weekday holiday")
  print(p); save_slide(p, paste0(prefix, "_B6_around_holidays"))
  
  # B7 - Penultimo e terzultimo giorno feriale: venerdì e altri giorni ---------
  b7 <- d %>% dplyr::filter(to_eom %in% 1:2, wd <= 5) %>%
    mutate(x   = factor(to_eom, 1:2, c("EoM-1", "EoM-2")),
           day = factor(ifelse(wd == 5, "Friday", "Monday-Thursday"),
                        levels = c("Monday-Thursday", "Friday")))
  cat("\nB7 - EoM-1 and EoM-2 on working days: Friday vs other days\n"); print_medians(b7, c("x", "day"))
  p <- box_dmr(b7, "x", "Days to the end of the month (working days)", groups = "x", fill_var = "day")
  print(p); save_slide(p, paste0(prefix, "_B7_last_working_day"))
  
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Situazione "prima": calendario base
# ------------------------------------------------------------------------------
make_dmr_boxplots("Basic calendar", "basic")