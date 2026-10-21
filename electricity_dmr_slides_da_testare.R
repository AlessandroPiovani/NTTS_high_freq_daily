################################################################################
#                                                                              #
#   DOMANDA ELETTRICA - IMMAGINI DEI DMR PER LE SLIDE                          #
#                                                                              #
################################################################################
# Da lanciare DOPO lo script completo dell'elettricità (Italia). Servono:
#   data        data.frame con Date e Value (serie grezza, MWh)
#   log         FALSE per l'elettricità (modello additivo)
#   daily       data.frame con Date e SA_CA (calendario finale, coefficienti
#               variabili nel tempo)
# Se usi i calendar tools (run_calendar_sa), metti prima:
#   sa_ca_final_el <- res_el$sa_ca
# Non modifica nessun oggetto dello script principale.
#
# Ogni immagine confronta il DMR con il calendario BASE e con quello FINALE:
#   calendario base   = 12 festività nazionali attive in TUTTI i giorni
#                       (type = "All"), coefficienti fissi, nessun altro
#                       regressore: il punto di partenza "standard"
#   calendario finale = i 23 regressori, coefficienti variabili nel tempo
#   DMR = SA_CA del giorno / mediana della SA_CA del suo mese (atteso ~1)
#
# Immagini (una per meccanismo, sempre base vs finale):
#   E1 giorno del mese            -> nessun effetto intra-mensile (a differenza
#                                    della fatturazione): la diagnostica si
#                                    trasferisce, il calendario no
#   E2 la festività, per giorno   -> festività nel weekend sovra-corrette,
#      della settimana               festività infrasettimanali sotto-corrette
#   E3 attorno alle festività     -> ponti: lunedì prima di una festa di
#      (per giorno della festa)      martedì, venerdì dopo una di giovedì
#   E4 finestra di Pasqua         -> il calo va dal Venerdì Santo al martedì
#   E5 periodo natalizio          -> effetti a data fissa ma solo nei feriali:
#   E6 agosto                        la stagionalità annuale ne prende la media
#
# Stile: boxplot affiancati, arancio = base, blu = finale (Okabe-Ito); fascia
# grigia = tolleranza ±5%, linea tratteggiata = 1; nessun titolo
# nell'immagine; stesse dimensioni e caratteri delle immagini della
# fatturazione. Ogni immagine stampa a console le mediane disegnate; alla fine
# una tabella riassume quanti gruppi escono dalla tolleranza (base vs finale).
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# ------------------------------------------------------------------------------
# 0a) Impostazioni comuni (come per la fatturazione)
# ------------------------------------------------------------------------------
out_dir   <- "slides_dmr_electricity"
fig_w_cm  <- 24
fig_h_cm  <- 11
base_size <- 16
tol_dmr   <- 0.05                          # tolleranza attorno a 1

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
          strip.text         = element_text(face = "bold", size = base_size),
          plot.margin        = margin(5, 10, 5, 5))
}

save_slide <- function(p, name, w = fig_w_cm, h = fig_h_cm) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h, units = "cm", dpi = 300)
  ggsave(file.path(out_dir, paste0(name, ".pdf")), p, width = w, height = h, units = "cm")
  cat("Saved:", file.path(out_dir, paste0(name, c(".png", ".pdf"))), "\n")
}

tolerance_layers <- list(
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1 - tol_dmr, ymax = 1 + tol_dmr,
           fill = "grey88", alpha = 0.8),
  geom_hline(yintercept = 1, linetype = 2, color = "grey40")
)

# ------------------------------------------------------------------------------
# 0b) Serie e calendario delle festività
# ------------------------------------------------------------------------------
dates_el <- data$Date
y_el     <- data$Value
n_el     <- length(dates_el)
if (!exists("sa_ca_final_el")) {
  stopifnot(all(daily$Date == dates_el))
  sa_ca_final_el <- daily$SA_CA
}
stopifnot(length(sa_ca_final_el) == n_el, all(diff(dates_el) == 1))

it_cal_el <- rjd3toolkit::national_calendar(days = list(
  rjd3toolkit::fixed_day(4, 25),            # Liberation Day
  rjd3toolkit::fixed_day(6, 2),             # Republic Day
  rjd3toolkit::fixed_day(12, 8),            # Immaculate Conception
  rjd3toolkit::fixed_day(12, 26),           # St. Stephen's Day
  rjd3toolkit::fixed_day(1, 6),             # Epiphany
  rjd3toolkit::special_day('NEWYEAR'),      # New Year's Day
  rjd3toolkit::special_day('EASTERMONDAY'), # Easter Monday
  rjd3toolkit::special_day('MAYDAY'),       # Labour Day
  rjd3toolkit::special_day('ASSUMPTION'),   # Assumption of Mary
  rjd3toolkit::special_day('ALLSAINTSDAY'), # All Saints' Day
  rjd3toolkit::special_day('CHRISTMAS'),    # Christmas
  rjd3toolkit::special_day('EASTER')        # Easter Sunday
))
hol_el <- as.matrix(rjd3toolkit::holidays(calendar = it_cal_el, start = min(dates_el), length = n_el,
                                          type = "All", nonworking = c(6, 7)))
colnames(hol_el) <- c("Liberation Day", "Republic Day", "Immaculate Conception", "St. Stephen's Day",
                      "Epiphany", "New Year's Day", "Easter Monday", "Labour Day",
                      "Assumption of Mary", "All Saints' Day", "Christmas", "Easter Sunday")

wd_el    <- wday(dates_el, week_start = 1)                    # 1 = lunedì ... 7 = domenica
md_el    <- format(dates_el, "%m-%d")
is_hol   <- rowSums(hol_el[, colnames(hol_el) != "Easter Sunday"]) > 0   # 11 festività, ogni giorno
em_el    <- hol_el[, "Easter Monday"] == 1
xmas_win <- md_el >= "12-20" | md_el <= "01-10"
wd_lab   <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

# ------------------------------------------------------------------------------
# 0c) SA_CA con il calendario BASE (12 festività type "All", coefficienti fissi)
# ------------------------------------------------------------------------------
if (!exists("sa_ca_base_el")) {
  pre_pro_base_el <- rjd3highfreq::fractionalAirlineEstimation(
    y = y_el, x = hol_el, log = log, criticalValue = 6,
    periods = c(7, 365.2425), outliers = c("ao", "wo", "ls"), y_time = dates_el)
  dow_base_el <- rjd3highfreq::fractionalAirlineDecomposition(
    y = pre_pro_base_el$model$linearized, period = 7, y_time = dates_el)
  doy_base_el <- rjd3highfreq::fractionalAirlineDecomposition(
    y = dow_base_el$decomposition$sa, period = 365.2425, y_time = dates_el)

  sa_ca_base_el <- doy_base_el$decomposition$sa
  ls_b <- pre_pro_base_el$model$component_ls
  if (!is.null(ls_b) && length(ls_b) == n_el && !all(is.na(ls_b))) {
    sa_ca_base_el <- if (log) sa_ca_base_el + log(ls_b) else sa_ca_base_el + ls_b
  }
  if (log) sa_ca_base_el <- exp(sa_ca_base_el)
}

# ------------------------------------------------------------------------------
# 0d) Tabella unica dei DMR (base e finale)
# ------------------------------------------------------------------------------
make_dmr <- function(dates, sa_ca, label) {
  data.frame(Date = dates, SA_CA = as.numeric(sa_ca)) %>%
    mutate(month    = floor_date(Date, "month"),
           dom      = day(Date),
           wd       = wday(Date, week_start = 1),
           md       = format(Date, "%m-%d"),
           day_kind = factor(ifelse(wd <= 5, "Monday-Friday", "Saturday-Sunday"),
                             levels = c("Monday-Friday", "Saturday-Sunday")),
           calendar = label) %>%
    group_by(month) %>%
    mutate(DMR = SA_CA / median(SA_CA)) %>%
    ungroup()
}

dmr_el <- bind_rows(make_dmr(dates_el, sa_ca_base_el,  "Basic calendar"),
                    make_dmr(dates_el, sa_ca_final_el, "Final calendar")) %>%
  mutate(calendar = factor(calendar, levels = names(cal_cols)))

# ------------------------------------------------------------------------------
# 0e) Funzioni: limiti y, boxplot affiancati, tabella delle mediane
# ------------------------------------------------------------------------------
# Limiti dell'asse y dai baffi dei boxplot, sempre inclusa la fascia ±5%
whisker_ylim <- function(df, groups) {
  w <- df %>%
    group_by(across(all_of(groups))) %>%
    summarise(q1 = quantile(DMR, 0.25), q3 = quantile(DMR, 0.75),
              lo = max(min(DMR), q1 - 1.5 * (q3 - q1)),
              hi = min(max(DMR), q3 + 1.5 * (q3 - q1)), .groups = "drop")
  yl <- c(min(w$lo, 1 - tol_dmr), max(w$hi, 1 + tol_dmr))
  yl + c(-1, 1) * 0.04 * diff(yl)
}

# Boxplot base (arancio) e finale (blu) affiancati per ogni gruppo di giorni
box_cmp <- function(df, xvar, xlab, facet = NULL, facet_nrow = 1, rotate_x = FALSE) {
  yl <- whisker_ylim(df, unique(c(xvar, "calendar", facet)))
  n_out <- sum(df$DMR < yl[1] | df$DMR > yl[2])
  if (n_out > 0) cat("Note:", n_out, "extreme values outside the plotted y range\n")

  p <- ggplot(df, aes(x = .data[[xvar]], y = DMR, fill = calendar, color = calendar)) +
    tolerance_layers +
    geom_boxplot(width = 0.75, position = position_dodge(0.85), alpha = 0.30,
                 linewidth = 0.45, outlier.size = 0.6, outlier.alpha = 0.5) +
    scale_fill_manual(values = cal_cols) +
    scale_color_manual(values = cal_cols) +
    coord_cartesian(ylim = yl) +
    labs(x = xlab, y = "DMR") +
    theme_slides()
  if (!is.null(facet)) p <- p + facet_wrap(as.formula(paste("~", facet)), nrow = facet_nrow)
  if (rotate_x) p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p
}

# Mediane per gruppo, base e finale affiancati; le salva per il riepilogo finale
medians_all <- list()
print_cmp <- function(df, groups, image) {
  m <- df %>%
    group_by(across(all_of(c(groups, "calendar")))) %>%
    summarise(median_DMR = round(median(DMR), 3), n = n(), .groups = "drop") %>%
    pivot_wider(names_from = calendar, values_from = median_DMR)
  print(as.data.frame(m))
  medians_all[[image]] <<- m
  invisible(m)
}

# Righe di dmr_el per un insieme di (Date, etichette): base e finale insieme
pick_days <- function(tab) inner_join(tab, dmr_el, by = "Date")


################################################################################
#  E1 - GIORNO DEL MESE                                                        #
################################################################################
# Messaggio: "Same diagnostic, different answer: no intra-month effect in
# electricity demand" (profilo piatto già con il calendario base)
# ------------------------------------------------------------------------------
e1 <- dmr_el %>% mutate(x = factor(dom, 1:31))
cat("\nE1 - Day of the month\n"); print_cmp(e1, "x", "E1")
p <- box_cmp(e1, "x", "Day of the month") +
  scale_x_discrete(breaks = as.character(c(1, 5, 10, 15, 20, 25, 31)))
save_slide(p, "E1_day_of_month")


################################################################################
#  E2 - LA FESTIVITÀ, PER GIORNO DELLA SETTIMANA IN CUI CADE                   #
################################################################################
# Messaggio: con il calendario base le festività del weekend sono sovra-
# corrette (DMR > 1: si toglie un calo da giorno feriale a un giorno già
# basso) e quelle di metà settimana sotto-corrette (DMR < 1).
# 11 festività (Pasqua esclusa), in qualsiasi giorno della settimana.
# ------------------------------------------------------------------------------
e2 <- pick_days(data.frame(Date = dates_el[is_hol])) %>% mutate(x = factor(wd, 1:7, wd_lab))
cat("\nE2 - Holiday itself, by weekday of the holiday\n"); print_cmp(e2, "x", "E2")
p <- box_cmp(e2, "x", "Weekday of the holiday")
save_slide(p, "E2_holiday_by_weekday")


################################################################################
#  E3 - ATTORNO ALLE FESTIVITÀ FERIALI, PER GIORNO DELLA FESTA                 #
################################################################################
# Messaggio: i ponti. Per una festa di martedì H-1 è il lunedì di ponte, per
# una di giovedì H+1 è il venerdì di ponte.
# Esclusi il periodo 20/12-10/01 (immagine E5) e Pasquetta (immagine E4).
# ------------------------------------------------------------------------------
h_idx  <- which(is_hol & wd_el <= 5 & !xmas_win & !em_el)
h_type <- factor(case_when(wd_el[h_idx] == 2 ~ "Tuesday holiday",
                           wd_el[h_idx] == 4 ~ "Thursday holiday",
                           TRUE              ~ "Mon, Wed or Fri holiday"),
                 levels = c("Tuesday holiday", "Thursday holiday", "Mon, Wed or Fri holiday"))
around <- bind_rows(lapply(-3:3, function(k) {
  ok <- h_idx + k >= 1 & h_idx + k <= n_el
  data.frame(Date = dates_el[h_idx[ok] + k], k = k, h_type = h_type[ok])
}))
cat("\nNumber of holidays by type:\n"); print(table(h_type))
e3 <- pick_days(around) %>%
  mutate(x = factor(k, -3:3, c("H-3", "H-2", "H-1", "H", "H+1", "H+2", "H+3")))
cat("\nE3 - Around weekday holidays, by weekday of the holiday\n"); print_cmp(e3, c("h_type", "x"), "E3")
p <- box_cmp(e3, "x", "Days around the holiday (H)", facet = "h_type")
save_slide(p, "E3_around_holidays", h = fig_h_cm + 1)


################################################################################
#  E4 - FINESTRA DI PASQUA                                                     #
################################################################################
# Messaggio: il calo non è solo Pasqua e Pasquetta, va dal Venerdì Santo al
# martedì; il giovedì santo e il mercoledì dopo fanno da controllo.
# ------------------------------------------------------------------------------
em_idx <- which(em_el)
easter_lab <- c("Holy Thu", "Good Fri", "Holy Sat", "Easter Sun", "Easter Mon", "Easter Tue", "Easter Wed")
e4 <- pick_days(bind_rows(lapply(-4:2, function(k) {
  ok <- em_idx + k >= 1 & em_idx + k <= n_el
  data.frame(Date = dates_el[em_idx[ok] + k], k = k)
}))) %>% mutate(x = factor(k, -4:2, easter_lab))
cat("\nE4 - Easter window\n"); print_cmp(e4, "x", "E4")
p <- box_cmp(e4, "x", NULL)
save_slide(p, "E4_easter_window")


################################################################################
#  E5 - PERIODO NATALIZIO (20/12 - 07/01), FERIALI E WEEKEND                   #
################################################################################
# Messaggio: giorni a data fissa ma con un effetto solo nei feriali. La
# stagionalità annuale ne prende la media: con il calendario base i feriali
# restano sotto 1, i weekend vanno sopra 1.
# ------------------------------------------------------------------------------
xmas_md  <- c(sprintf("12-%02d", 20:31), sprintf("01-%02d", 1:7))
xmas_lab <- c(sprintf("%02d/12", 20:31), sprintf("%02d/01", 1:7))
e5 <- dmr_el %>% dplyr::filter(md %in% xmas_md) %>% mutate(x = factor(md, xmas_md, xmas_lab))
cat("\nE5 - Christmas period, by date and day kind\n"); print_cmp(e5, c("day_kind", "x"), "E5")
p <- box_cmp(e5, "x", NULL, facet = "day_kind", facet_nrow = 2, rotate_x = TRUE)
save_slide(p, "E5_christmas_period", h = fig_h_cm + 4)


################################################################################
#  E6 - AGOSTO (08/08 - 22/08), FERIALI E WEEKEND                              #
################################################################################
# Messaggio: stesso meccanismo attorno a Ferragosto (14 e 16 agosto feriali).
# ------------------------------------------------------------------------------
aug_md  <- sprintf("08-%02d", 8:22)
aug_lab <- sprintf("%02d/08", 8:22)
e6 <- dmr_el %>% dplyr::filter(md %in% aug_md) %>% mutate(x = factor(md, aug_md, aug_lab))
cat("\nE6 - Mid-August, by date and day kind\n"); print_cmp(e6, c("day_kind", "x"), "E6")
p <- box_cmp(e6, "x", NULL, facet = "day_kind", facet_nrow = 2, rotate_x = TRUE)
save_slide(p, "E6_mid_august", h = fig_h_cm + 4)


################################################################################
#  RIEPILOGO - GRUPPI FUORI TOLLERANZA, BASE VS FINALE                         #
################################################################################
# Per ogni immagine: numero di gruppi di giorni, quanti hanno la mediana del
# DMR fuori da 1 ± 5%, e il peggiore. Serve per la slide di sintesi.
# ------------------------------------------------------------------------------
summary_el <- bind_rows(lapply(names(medians_all), function(im) {
  m <- medians_all[[im]]
  data.frame(image          = im,
             groups         = nrow(m),
             out_basic      = sum(abs(m[["Basic calendar"]] - 1) > tol_dmr),
             out_final      = sum(abs(m[["Final calendar"]] - 1) > tol_dmr),
             max_dev_basic  = round(max(abs(m[["Basic calendar"]] - 1)), 3),
             max_dev_final  = round(max(abs(m[["Final calendar"]] - 1)), 3))
}))
cat("\nSummary: groups with median DMR outside 1 ± ", tol_dmr, "\n", sep = "")
print(summary_el)
