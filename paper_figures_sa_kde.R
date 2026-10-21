################################################################################
#                                                                              #
#   FIGURE PER SLIDE E PAPER: GREZZO VS DESTAGIONALIZZATA E KDE DELLE ACF      #
#   Fatturazione elettronica ed elettricità, separate e in un'unica figura     #
#                                                                              #
################################################################################
# Parte dai due workspace salvati alla fine degli script finali:
#   workspace_electronic_payments_LOG_SUM.RData   (fatturazione elettronica)
#   workspace_electricity_demand_IT.RData         (elettricità)
# Ognuno viene caricato in un ambiente separato, così gli oggetti con lo
# stesso nome (data, daily, log, ...) non si sovrascrivono.
# Da ogni workspace prende: data$Date, data$Value (grezzo), daily$SA_CA
# (calendario finale) e log. La SA_CA con il calendario BASE si usa se è già
# nel workspace (sa_ca_base / sa_ca_base_el), altrimenti si stima qui con lo
# stesso calendario base delle slide DMR:
#   fatturazione  12 festività (type = "All") + EoM, EoM-1, EoM-2
#   elettricità   12 festività (type = "All")
# (coefficienti fissi, outlier AO/WO/LS con valore critico 6, AMB sequenziale).
#
# Figure prodotte in fig_dir (PNG 300 dpi + PDF vettoriale):
#   raw_sa_einvoicing, raw_sa_electricity, raw_sa_both
#       grezzo (giallo) e destagionalizzata e corretta per il calendario
#       (verde, calendario finale), come nei grafici "Sequential AMB"
#   kde_einvoicing, kde_electricity, kde_both
#       densità (KDE) delle autocorrelazioni mobili ai ritardi 7, 30 e 365:
#       grezzo (grigio), SA_CA con calendario base (arancio), SA_CA con
#       calendario finale (blu)
#
# Autocorrelazioni mobili (KDE)
#   - finestre mobili di window_cycles x ritardo giorni (4 x 7 = 28 giorni,
#     3 x 30 = 90 giorni, 2 x 365 = 730 giorni), spostate di un giorno alla
#     volta (stessi ritardi e finestre di default di plot_kde_acf);
#   - il ritardo 30 serve per la fatturazione (effetti intra-mensili); per
#     l'elettricità, senza effetti nel mese, fa da controllo;
#   - in ogni finestra si calcola l'autocorrelazione al ritardo (stimatore
#     usuale di acf(), media della finestra tolta);
#   - per default sulle DIFFERENZE PRIME (dei log per la fatturazione, che è in
#     log): così il trend non gonfia le autocorrelazioni, e una buona
#     destagionalizzata ha densità centrata su 0, mentre una stagionalità o un
#     effetto di calendario residuo sposta la densità verso valori positivi
#     o la allarga. Con transform = "level" si usano i livelli: stesso
#     calcolo di plot_kde_acf() di utilities.R (acf() su ogni finestra).
#   - Una densità più stretta e centrata su 0 = destagionalizzazione migliore.
#     Le differenze tra base e finale possono essere piccole: il KDE misura la
#     stagionalità residua di tutta la serie, mentre il calendario corregge
#     pochi giorni l'anno. I numeri stampati a console aiutano a leggerle.
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------------------------
# 0) Impostazioni
# ------------------------------------------------------------------------------
fig_dir   <- "figures_paper"
base_size <- 11                      # corpo del testo (punti) nelle figure
transform <- "diff"                  # "diff" (consigliato) oppure "level"
lags      <- c(7, 30, 365)
win_cyc   <- c(4, 3, 2)              # finestre: 4 settimane, 3 mesi, 2 anni
kde_band  <- 0.025                  # fascia grigia attorno a 0 come in plot_kde_acf (NULL = niente)

# Serie: etichetta e unità sull'asse y (la scala log viene dal workspace)
series_info <- list(
  einvoicing  = list(label = "E-invoicing",
                     units = "million euro", divide_by = 1e6),   # <-- verifica unità e divisore
  electricity = list(label = "Electricity demand",
                     units = "thousand MWh", divide_by = 1e3)
)

col_raw   <- "#F1C232"               # grezzo (giallo, come nello script)
col_sa    <- "#006400"               # destagionalizzata (verde, come nello script)
kde_cols  <- c("Raw"            = "grey55",
               "Basic calendar" = "#D55E00",
               "Final calendar" = "#0072B2")

dir.create(fig_dir, showWarnings = FALSE)

save_fig <- function(p, name, w, h) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, units = "cm", dpi = 300)
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h, units = "cm")
  cat("Saved:", file.path(fig_dir, paste0(name, c(".png", ".pdf"))), "\n")
}

theme_paper <- function() {
  theme_classic(base_size = base_size) +
    theme(legend.position  = "top",
          legend.title     = element_blank(),
          legend.margin    = margin(0, 0, -4, 0),
          strip.background = element_blank(),
          strip.text       = element_text(face = "bold", size = base_size),
          panel.spacing    = unit(0.6, "cm"),
          plot.margin      = margin(4, 8, 4, 4))
}

# ------------------------------------------------------------------------------
# 1) Lettura delle serie dai workspace
# ------------------------------------------------------------------------------
ws_files <- c(einvoicing  = "workspace_electronic_payments_LOG_SUM.RData",
              electricity = "workspace_electricity_demand_IT.RData")

# SA_CA con un calendario a coefficienti fissi (stima congiunta, poi AMB)
basic_sa_ca <- function(dates, y, X, log) {
  pp  <- rjd3highfreq::fractionalAirlineEstimation(
    y = y, x = X, log = log, criticalValue = 6,
    periods = c(7, 365.2425), outliers = c("ao", "wo", "ls"), y_time = dates)
  dow <- rjd3highfreq::fractionalAirlineDecomposition(
    y = pp$model$linearized, period = 7, y_time = dates)
  doy <- rjd3highfreq::fractionalAirlineDecomposition(
    y = dow$decomposition$sa, period = 365.2425, y_time = dates)
  sa <- doy$decomposition$sa
  ls <- pp$model$component_ls
  if (!is.null(ls) && length(ls) == length(y) && !all(is.na(ls))) sa <- if (log) sa + log(ls) else sa + ls
  if (log) exp(sa) else sa
}

# Fine mese: EoM, EoM-1, ..., EoM-k (riserva, se generate_eom_regressors
# del workspace non è utilizzabile)
eom_dummies <- function(dates, k = 2) {
  to_eom <- as.integer(lubridate::days_in_month(dates)) - lubridate::day(dates)
  sapply(0:k, function(j) as.numeric(to_eom == j))
}

it_cal_default <- rjd3toolkit::national_calendar(days = list(
  rjd3toolkit::fixed_day(4, 25), rjd3toolkit::fixed_day(6, 2), rjd3toolkit::fixed_day(12, 8),
  rjd3toolkit::fixed_day(12, 26), rjd3toolkit::fixed_day(1, 6),
  rjd3toolkit::special_day('NEWYEAR'), rjd3toolkit::special_day('EASTERMONDAY'),
  rjd3toolkit::special_day('MAYDAY'), rjd3toolkit::special_day('ASSUMPTION'),
  rjd3toolkit::special_day('ALLSAINTSDAY'), rjd3toolkit::special_day('CHRISTMAS'),
  rjd3toolkit::special_day('EASTER')))

load_series <- function(k) {
  env <- new.env()
  load(ws_files[[k]], envir = env)
  d  <- env$data
  dl <- env$daily
  stopifnot(!is.null(d), !is.null(dl), all(dl$Date == d$Date), all(diff(d$Date) == 1))
  lg <- env$log

  base_name <- c(einvoicing = "sa_ca_base", electricity = "sa_ca_base_el")[[k]]
  if (exists(base_name, envir = env, inherits = FALSE)) {
    cat(k, ": basic-calendar SA_CA taken from the workspace (", base_name, ")\n", sep = "")
    sa_b <- get(base_name, envir = env)
  } else {
    cat(k, ": estimating the basic-calendar SA_CA ...\n", sep = "")
    cal <- if (exists("italian_calendar", envir = env, inherits = FALSE)) env$italian_calendar else it_cal_default
    X <- as.matrix(rjd3toolkit::holidays(calendar = cal, start = min(d$Date), length = nrow(d),
                                         type = "All", nonworking = c(6, 7)))
    colnames(X) <- paste0("holiday_", seq_len(ncol(X)))
    if (k == "einvoicing") {
      eom <- tryCatch(as.matrix(env$generate_eom_regressors(d$Date, num_days_before = 2)),
                      error = function(e) { cat("  (EoM regressors built here)\n"); eom_dummies(d$Date, 2) })
      colnames(eom) <- paste0("eom_minus_", seq_len(ncol(eom)) - 1)
      X <- cbind(X, eom)
    }
    stopifnot(all(colSums(X != 0) > 0), qr(X)$rank == ncol(X))
    sa_b <- basic_sa_ca(d$Date, d$Value, X, lg)
  }
  stopifnot(length(sa_b) == nrow(d))
  list(log = lg,
       data = data.frame(Date = d$Date, Raw = d$Value,
                         SA_CA_basic = as.numeric(sa_b), SA_CA_final = as.numeric(dl$SA_CA)))
}

loaded <- lapply(names(ws_files), load_series)
names(loaded) <- names(ws_files)
series <- lapply(loaded, `[[`, "data")
for (k in names(series)) series_info[[k]]$log <- loaded[[k]]$log   # log come nel modello

# ------------------------------------------------------------------------------
# 2) GREZZO VS DESTAGIONALIZZATA
# ------------------------------------------------------------------------------
# Scala dell'asse y: logaritmica per le serie stimate in log (fatturazione),
# altrimenti i picchi di fine mese schiacciano la destagionalizzata sul fondo.
# Per la figura unica i due grafici sono messi uno sotto l'altro con grid
# (ognuno con la sua scala), la legenda solo in quello in alto.
raw_sa_plot <- function(k, legend = TRUE) {
  s <- series_info[[k]]; d <- series[[k]]
  df <- data.frame(Date = d$Date, Raw = d$Raw / s$divide_by, SA = d$SA_CA_final / s$divide_by) %>%
    pivot_longer(c(Raw, SA), names_to = "what", values_to = "value") %>%
    mutate(what = factor(what, c("Raw", "SA"), c("Raw", "Seasonally and calendar adjusted")))
  log_y <- isTRUE(s$log)
  if (log_y) df <- df %>% dplyr::filter(value > 0)      # su scala log servono valori positivi
  p <- ggplot(df, aes(Date, value, color = what)) +
    geom_line(linewidth = 0.25) +
    scale_color_manual(values = c(col_raw, col_sa)) +
    labs(x = NULL, y = paste0(s$label, " (", s$units, if (log_y) ", log scale", ")")) +
    guides(color = guide_legend(override.aes = list(linewidth = 1.2))) +
    theme_paper()
  if (log_y) p <- p + scale_y_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE))
  if (!legend) p <- p + theme(legend.position = "none")
  p
}

# Più grafici ggplot uno sotto l'altro, salvati in PNG e PDF
save_stack <- function(plots, heights, name, w, h) {
  draw <- function() {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(plots), 1,
                                      heights = grid::unit(heights, "null"))))
    for (i in seq_along(plots))
      print(plots[[i]], vp = grid::viewport(layout.pos.row = i, layout.pos.col = 1))
  }
  png(file.path(fig_dir, paste0(name, ".png")), width = w, height = h, units = "cm", res = 300)
  draw(); dev.off()
  pdf(file.path(fig_dir, paste0(name, ".pdf")), width = w / 2.54, height = h / 2.54)
  draw(); dev.off()
  cat("Saved:", file.path(fig_dir, paste0(name, c(".png", ".pdf"))), "\n")
}

# 2a) Figure singole
for (k in names(series)) save_fig(raw_sa_plot(k), paste0("raw_sa_", k), w = 16, h = 8)

# 2b) Figura unica: fatturazione sopra (con legenda), elettricità sotto
save_stack(list(raw_sa_plot("einvoicing"), raw_sa_plot("electricity", legend = FALSE)),
           heights = c(1.12, 1), name = "raw_sa_both", w = 16, h = 15)

# ------------------------------------------------------------------------------
# 3) KDE DELLE AUTOCORRELAZIONI MOBILI: GREZZO, CALENDARIO BASE, FINALE
# ------------------------------------------------------------------------------
# Autocorrelazione al ritardo 'lag' in finestre mobili di 'window' giorni
roll_acf <- function(x, lag, window) {
  ends <- seq(window, length(x))
  vapply(ends, function(e) {
    w <- x[(e - window + 1):e]
    if (anyNA(w)) return(NA_real_)
    w <- w - mean(w)
    sum(w[1:(window - lag)] * w[(lag + 1):window]) / sum(w^2)
  }, numeric(1))
}

prep <- function(x, log) {
  if (log) x <- log(x)
  if (transform == "diff") diff(x) else x
}

kde_long <- bind_rows(lapply(names(series), function(k) {
  s <- series_info[[k]]; d <- series[[k]]
  vers <- list("Raw" = d$Raw, "Basic calendar" = d$SA_CA_basic, "Final calendar" = d$SA_CA_final)
  bind_rows(lapply(seq_along(lags), function(j) {
    bind_rows(lapply(names(vers), function(v) {
      data.frame(series  = s$label,
                 lag     = paste0("Lag ", lags[j], " (", lags[j] * win_cyc[j], "-day windows)"),
                 version = v,
                 acf     = roll_acf(prep(vers[[v]], s$log), lags[j], lags[j] * win_cyc[j]))
    }))
  }))
})) %>%
  dplyr::filter(!is.na(acf)) %>%
  mutate(series  = factor(series, sapply(series_info, `[[`, "label")),
         lag     = factor(lag, unique(lag)),
         version = factor(version, names(kde_cols)))

# Sintesi numerica: centro e ampiezza della distribuzione delle ACF mobili
kde_stats <- kde_long %>%
  group_by(series, lag, version) %>%
  summarise(median   = round(median(acf), 3),
            p10      = round(quantile(acf, 0.10), 3),
            p90      = round(quantile(acf, 0.90), 3),
            mean_abs = round(mean(abs(acf)), 3),
            windows  = n(), .groups = "drop")
cat("\nRolling ACF (transform = ", transform, "): closer to 0 and narrower = better\n", sep = "")
print(as.data.frame(kde_stats))

plot_kde <- function(df, grid = FALSE) {
  p <- ggplot(df, aes(acf, color = version))
  if (!is.null(kde_band))
    p <- p + annotate("rect", xmin = -kde_band, xmax = kde_band, ymin = 0, ymax = Inf,
                      fill = "grey80", alpha = 0.4)
  p <- p +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    geom_density(linewidth = 0.7) +
    scale_color_manual(values = kde_cols) +
    labs(x = "Rolling autocorrelation", y = "Density") +
    guides(color = guide_legend(override.aes = list(linewidth = 1.2))) +
    theme_paper()
  if (grid) p + facet_grid(series ~ lag, scales = "free")
  else      p + facet_wrap(~ lag, nrow = 1, scales = "free")
}

# 3a) Figure singole: per serie, ritardi 7, 30 e 365 affiancati
for (k in names(series)) {
  df <- kde_long %>% dplyr::filter(series == series_info[[k]]$label)
  save_fig(plot_kde(df), paste0("kde_", k), w = 18, h = 7.5)
}

# 3b) Figura unica: righe = serie, colonne = ritardi
save_fig(plot_kde(kde_long, grid = TRUE), "kde_both", w = 18, h = 13)
