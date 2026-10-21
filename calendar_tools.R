################################################################################
#  STRUMENTI PER GLI EFFETTI DI CALENDARIO VARIABILI NEL TEMPO                 #
#  Funzioni generiche, valide per qualsiasi serie e qualsiasi calendario.      #
################################################################################
# Contiene quattro funzioni:
#   estimate_tv_calendar()     stima i coefficienti di calendario variabili nel
#                              tempo (e, per confronto, fissi) con il modello in
#                              due passi di rjd3sts
#   plot_calendar_figure_a4()  figura per il paper: uno o più pannelli uno sotto
#                              l'altro, in PNG (300 dpi) e PDF, già della misura
#                              finale in centimetri
#   run_calendar_sa()          destagionalizzazione completa: calendario (fisso o
#                              variabile), outlier, AMB sequenziale
#   dmr_diagnostics()          diagnostica del calendario sulla SA_CA (DMR)
# Sostituiscono plot_calendar_paths() e .draw_cal_group(), che non servono più:
# per un grafico singolo si usa plot_calendar_figure_a4() con un solo gruppo.
#
# Pacchetti: rjd3sts, rjd3toolkit, rjd3highfreq, lubridate.
#
# Convenzione del paper
#   serie in log   -> "Calendar effect (multiplicative factor)" = exp(beta),
#                     asse logaritmico, riferimento a 1
#   serie additive -> beta nelle unità naturali, riferimento a 0
################################################################################


# ------------------------------------------------------------------------------
# 1) STIMA
# ------------------------------------------------------------------------------
# Modello: airline sul periodo indicato (default settimanale) + regressori di
# calendario con coefficienti che seguono una passeggiata casuale, con una
# varianza comune stimata (var_init è solo il valore iniziale).
# Stima anche lo stesso modello con coefficienti fissi (varianza zero), per il
# test di verosimiglianza e per un eventuale confronto.
#
# Argomenti
#   y         serie (vettore numerico, positiva se log = TRUE)
#   X         matrice dei regressori di calendario, con i nomi delle colonne
#   log       TRUE = modello in log, FALSE = additivo
#   period    periodo dell'airline (7 per i dati giornalieri)
#   var_init  valore iniziale della varianza dei coefficienti
#
# Restituisce una lista:
#   beta        matrice n x k dei coefficienti variabili (scala del modello)
#   beta_fixed  vettore dei coefficienti fissi (scala del modello)
#   cal_effect  effetto di calendario complessivo, giorno per giorno
#               (scala del modello), per correggere la serie
#   ll_tv, ll_fx, LR   log-verosimiglianze e rapporto di verosimiglianza
# ------------------------------------------------------------------------------
estimate_tv_calendar <- function(y, X, log, period = 7, var_init = 0.1) {

  X <- as.matrix(X)
  stopifnot(length(y) == nrow(X), !is.null(colnames(X)))
  if (log && any(y <= 0, na.rm = TRUE)) stop("log = TRUE requires a strictly positive series")
  y_sts <- if (log) log(y) else y

  fit <- function(variable) {
    sm <- model()
    add(sm, sarima("airline", c(period), c(0, 1, 1), c(0, 1, 1)))
    if (variable) add(sm, reg("cal", X, var_init)) else add(sm, reg("cal", X))
    rjd3sts::estimate(sm, y_sts, marginal = F, initialization = "SqrtDiffuse",
                      optimizer = "LevenbergMarquardt", concentrated = T, precision = 1e-10)
  }
  r_tv <- fit(TRUE)
  r_fx <- fit(FALSE)

  # I regressori sono stati aggiunti per ultimi: i loro stati sono le ultime
  # k colonne, qualunque sia la dimensione dello stato dell'airline
  ss_tv <- result(r_tv, "ssf.smoothing.states")
  ss_fx <- result(r_fx, "ssf.smoothing.states")
  idx   <- (ncol(ss_tv) - ncol(X) + 1):ncol(ss_tv)

  beta <- ss_tv[, idx, drop = FALSE]
  colnames(beta) <- colnames(X)
  beta_fixed <- ss_fx[nrow(X), idx]
  names(beta_fixed) <- colnames(X)

  ll_tv <- tryCatch(result(r_tv, "likelihood.ll"), error = function(e) NA)
  ll_fx <- tryCatch(result(r_fx, "likelihood.ll"), error = function(e) NA)
  LR    <- 2 * (ll_tv - ll_fx)
  cat("Log-likelihood  fixed:", round(ll_fx, 1), " time-varying:", round(ll_tv, 1),
      " LR:", round(LR, 1), "(5% threshold ~2.71)\n")

  # Effetto di calendario complessivo, giorno per giorno (scala del modello):
  # serve per correggere la serie quando si sceglie il modello variabile.
  #   additivo: Value_td_adjusted = y - cal_effect
  #   log:      Value_td_adjusted = y / exp(cal_effect)
  cal_effect <- rowSums(X * beta)

  list(beta = beta, beta_fixed = beta_fixed, cal_effect = cal_effect,
       ll_tv = ll_tv, ll_fx = ll_fx, LR = LR)
}


# ------------------------------------------------------------------------------
# 2) FIGURA PER IL PAPER
# ------------------------------------------------------------------------------
# Argomenti principali
#   dates      date della serie
#   tv         risultato di estimate_tv_calendar()
#   groups     lista di vettori di nomi di regressori: un pannello per vettore
#              (un solo vettore = grafico singolo)
#   log        TRUE = fattori su asse logaritmico; FALSE = unità naturali
#   file_stub  nome dei file senza estensione (.png e .pdf)
#   main       titolo generale (facoltativo)
#   units, divide_by   solo per le serie additive (es. "thousand MWh", 1000)
#   width_cm, height_cm, pointsize   misura finale e corpo del testo
#   leg_cols   colonne della legenda: un valore per tutti o uno per gruppo
#   leg_row_factor, leg_pad_cm       per stringere o allargare le legende
# Scelte grafiche: una sola etichetta dell'asse y; nessun titolo nei pannelli;
# legende attaccate sotto ogni pannello; colori Okabe-Ito (oltre l'ottava voce
# si ripetono con linea tratteggiata); tacche logaritmiche simmetriche
# attorno a 1. Non apre finestre grafiche.
# ------------------------------------------------------------------------------
plot_calendar_figure_a4 <- function(dates, tv, groups, log, file_stub, main = NULL,
                                    units = "thousand MWh", divide_by = 1000,
                                    width_cm = 16, height_cm = 22, pointsize = 12,
                                    leg_cols = 3, leg_row_factor = 0.85, leg_pad_cm = 0) {

  # Nomi dei regressori: avvisa se qualcuno non esiste nel calendario
  missing <- setdiff(unlist(groups), colnames(tv$beta))
  if (length(missing) > 0) warning("Regressors not found and skipped: ", paste(missing, collapse = ", "))
  groups <- lapply(groups, function(v) intersect(v, colnames(tv$beta)))
  keep   <- lengths(groups) > 0
  groups <- groups[keep]

  # Colonne della legenda: un valore per tutti o uno per gruppo
  leg_cols <- rep_len(leg_cols, length(keep))[keep]

  conv <- function(b) if (log) exp(b) else b / divide_by
  ref  <- if (log) 1 else 0
  ylab <- if (log) "Calendar effect (multiplicative factor)" else paste0("Calendar effect (", units, ")")

  # Colori Okabe-Ito senza il giallo (si legge male su bianco)
  okabe <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  style <- function(n) list(col = okabe[(seq_len(n) - 1) %% 8 + 1],
                            lty = ifelse(seq_len(n) > 8, 2, 1))

  # Altezze: legende fisse in cm, grafici relativi (si dividono lo spazio restante).
  # Una riga di legenda è circa pointsize * 1,2 punti; leg_row_factor < 1 la
  # stringe (insieme a y.intersp), leg_pad_cm aggiunge spazio se serve.
  row_cm  <- pointsize * 1.2 / 72 * 2.54
  leg_cm  <- function(v, nc) ceiling(length(v) / nc) * row_cm * leg_row_factor + leg_pad_cm
  # L'ultima legenda tocca il bordo della figura: le si dà mezza riga in più,
  # altrimenti le lettere con la "coda" (g, p, y) dell'ultima riga vengono tagliate
  leg_h   <- unlist(Map(leg_cm, groups, leg_cols))
  leg_h[length(leg_h)] <- leg_h[length(leg_h)] + 0.5 * row_cm
  heights <- unlist(Map(function(h) c(1, lcm(h)), leg_h))

  # Tacche dell'asse logaritmico, simmetriche attorno a 1 (2 e 0,5; 1,5 e 0,67 ...).
  # Se l'intervallo è largo si passa a una serie più rada.
  log_ticks <- function(yl) {
    sym    <- function(b) sort(unique(c(1, b, 1 / b)))
    dense  <- sym(c(1.25, 1.5, 2, 3, 4, 5, 10, 20, 50, 100))
    sparse <- sym(c(2, 5, 10, 20, 50, 100))
    tk <- dense[dense >= yl[1] & dense <= yl[2]]
    if (length(tk) > 9) tk <- sparse[sparse >= yl[1] & sparse <= yl[2]]
    # Intervallo stretto (es. effetti tra 0,8 e 1): tacche regolari, es. 0,85 0,9 0,95 1
    if (length(tk) < 3) tk <- pretty(yl, n = 5)[pretty(yl, n = 5) >= yl[1] & pretty(yl, n = 5) <= yl[2]]
    tk
  }

  draw <- function() {
    op <- par(no.readonly = TRUE)
    on.exit({ layout(matrix(1)); par(op) })
    par(oma = c(0.2, 1.4, if (is.null(main)) 0.2 else 1.6, 0.2),
        mgp = c(1.8, 0.4, 0), tcl = -0.25, cex.axis = 1, las = 1)
    layout(matrix(seq_along(heights), ncol = 1), heights = heights)

    for (gi in seq_along(groups)) {
      vars <- groups[[gi]]
      y  <- conv(tv$beta[, vars, drop = FALSE])
      st <- style(length(vars))
      yl <- range(c(y, ref), na.rm = TRUE)
      yl <- if (log) yl * c(0.93, 1.07) else yl + c(-1, 1) * 0.06 * diff(yl)

      # Grafico: margini minimi (sotto solo lo spazio per gli anni)
      par(mar = c(1.5, 3, 0.15, 0.5))               # sopra quasi nulla: la legenda precedente è attaccata
      plot(dates, y[, 1], type = "n", ylim = yl, xlab = "", ylab = "",
           log = if (log) "y" else "", yaxt = if (log) "n" else "s", bty = "l")
      if (log) {
        tk <- log_ticks(yl)
        axis(2, at = tk, labels = as.character(signif(tk, 2)))
      }
      abline(h = ref, col = "grey55", lty = 3)
      for (k in seq_along(vars)) lines(dates, y[, k], col = st$col[k], lty = st$lty[k], lwd = 1.6)

      # Legenda subito sotto. Usa tutta la larghezza della figura (non solo quella
      # del grafico) e, se le voci sono troppo lunghe, riduce il testo quanto
      # basta per non uscire dai bordi (fino all'80%).
      par(mar = c(0, 0.3, 0, 0.3), xpd = NA)
      plot.new()
      labs   <- gsub("_", " ", vars)
      leg_fn <- function(cx, plot) legend("top", inset = c(0, -0.04), legend = labs,
                                          col = st$col, lty = st$lty, lwd = 1.8,
                                          ncol = leg_cols[gi], cex = cx, bty = "n",
                                          seg.len = 1.5, x.intersp = 0.5,
                                          y.intersp = leg_row_factor, plot = plot)
      cx <- 1
      while (leg_fn(cx, FALSE)$rect$w > 0.99 && cx > 0.8) cx <- cx - 0.025
      if (leg_fn(cx, FALSE)$rect$w > 0.99)
        warning("Legend too wide even at 80% text size: use fewer columns (leg_cols)")
      leg_fn(cx, TRUE)
      par(xpd = FALSE)
    }

    mtext(ylab, side = 2, outer = TRUE, line = 0.2, las = 0, cex = 1)
    if (!is.null(main)) mtext(main, side = 3, outer = TRUE, line = 0.3, cex = 1.1, font = 2)
  }

  # PNG a 300 dpi, già della misura finale
  png(paste0(file_stub, ".png"), width = width_cm, height = height_cm, units = "cm",
      res = 300, pointsize = pointsize)
  draw(); dev.off()

  # PDF vettoriale (consigliato per il paper)
  pdf(paste0(file_stub, ".pdf"), width = width_cm / 2.54, height = height_cm / 2.54,
      pointsize = pointsize)
  draw(); dev.off()

  cat("Saved:", paste0(file_stub, c(".png", ".pdf")), "\n")
  invisible(NULL)
}


################################################################################
#  ESEMPI D'USO                                                                #
################################################################################
# Turismo (log), figura a pagina intera con tre pannelli
#   cal_reg <- build_calendar_tourism(data$Date)
#   tv_tour <- estimate_tv_calendar(data$Value, cal_reg, log = TRUE)
#   plot_calendar_figure_a4(data$Date, tv_tour,
#     groups = list(grp_holidays, grp_bridges, grp_periods), log = TRUE,
#     main = "Tourist arrivals: time-varying calendar effects",
#     file_stub = "tourism_calendar_effects")
#
# Grafico singolo (un solo gruppo, figura più bassa)
#   plot_calendar_figure_a4(data$Date, tv_tour, groups = list(grp_holidays),
#     log = TRUE, height_cm = 9, file_stub = "tourism_cal_holidays")
#
# Elettricità (additiva, migliaia di MWh)
#   tv_el <- estimate_tv_calendar(data_el$Value, cal_reg_el, log = FALSE)
#   plot_calendar_figure_a4(data_el$Date, tv_el,
#     groups = list(colnames(cal_reg_el)[1:12], colnames(cal_reg_el)[13:23]),
#     log = FALSE, units = "thousand MWh", divide_by = 1000,
#     leg_cols = c(3, 2), height_cm = 16, file_stub = "electricity_calendar_effects")


################################################################################
# 3) DESTAGIONALIZZAZIONE COMPLETA (calendario -> outlier -> AMB)
################################################################################
# Stessa procedura per tutte le serie:
#   time_varying = FALSE  calendario stimato insieme al modello (coefficienti fissi)
#   time_varying = TRUE   calendario variabile nel tempo (estimate_tv_calendar),
#                         poi pre-trattamento solo per gli outlier
# Poi AMB sequenziale (settimana, poi anno), LS rimessi nella destagionalizzata.
#
# Restituisce una lista:
#   pre_pro     pre-trattamento (fractionalAirlineEstimation)
#   tv          risultato di estimate_tv_calendar (NULL se time_varying = FALSE)
#   cal_effect  effetto di calendario in livelli: fattore (log) o MWh/unità (additivo)
#   y_adj       serie corretta per il calendario
#   amb_dow, amb_doy   scomposizioni AMB
#   sa_ca       destagionalizzata e corretta per il calendario
#   sa          destagionalizzata con l'effetto di calendario rimesso
# ------------------------------------------------------------------------------
run_calendar_sa <- function(dates, y, X, log, time_varying = FALSE,
                            critical_value = 6, outliers = c("ao", "wo", "ls")) {

  X <- as.matrix(X)

  if (time_varying) {
    tv  <- estimate_tv_calendar(y, X, log)
    cal <- if (log) exp(tv$cal_effect) else tv$cal_effect
    y_adj <- if (log) y / cal else y - cal
    pre_pro <- rjd3highfreq::fractionalAirlineEstimation(
      y = y_adj, log = log, criticalValue = critical_value,
      periods = c(7, 365.2425), outliers = outliers, y_time = dates)
  } else {
    tv <- NULL
    pre_pro <- rjd3highfreq::fractionalAirlineEstimation(
      y = y, x = X, log = log, criticalValue = critical_value,
      periods = c(7, 365.2425), outliers = outliers, y_time = dates)
    comp <- pre_pro$model$component_userdef_reg_variables
    if (!is.null(comp) && length(comp) == length(y)) {
      comp <- as.numeric(comp)
      cal  <- if (log && abs(median(comp)) < 0.5) exp(comp) else comp   # in log: fattore
    } else {
      e   <- as.numeric(X %*% as.numeric(pre_pro$model$b[1:ncol(X)]))
      cal <- if (log) exp(e) else e
    }
    y_adj <- if (log) y / cal else y - cal
  }

  # AMB sequenziale sulla linearizzata (senza calendario e outlier)
  dow <- rjd3highfreq::fractionalAirlineDecomposition(
    y = pre_pro$model$linearized, period = 7, y_time = dates)
  doy <- rjd3highfreq::fractionalAirlineDecomposition(
    y = dow$decomposition$sa, period = 365.2425, y_time = dates)

  # LS: cambi di livello veri, si rimettono nella destagionalizzata
  sa <- doy$decomposition$sa
  ls <- pre_pro$model$component_ls
  if (!is.null(ls) && length(ls) == length(y)) sa <- if (log) sa + log(ls) else sa + ls
  sa_ca <- if (log) exp(sa) else sa
  sa_w  <- if (log) sa_ca * cal else sa_ca + cal

  list(pre_pro = pre_pro, tv = tv, cal_effect = cal, y_adj = y_adj,
       amb_dow = dow, amb_doy = doy, sa_ca = as.numeric(sa_ca), sa = as.numeric(sa_w))
}


################################################################################
# 4) DIAGNOSTICA DEL CALENDARIO SULLA SA_CA (DMR)
################################################################################
# DMR_t = SA_CA del giorno / mediana della SA_CA del suo mese (atteso ~1).
# Mediane del DMR per gruppi di giorni, uguali per tutte le serie:
#   1 giorno della settimana          5 giorno della festa: in settimana / weekend
#   2 giorno del mese                 6 settimana di Pasqua
#   3 giorni alla fine del mese       7 periodo natalizio (15/12-10/01)
#   4 attorno alle festività (-3..+3) 8 agosto
#     per giorno della settimana      9 date scelte (es. patroni locali), facoltativo
# Per ogni tabella stampa quanti gruppi sono fuori dalla tolleranza (tol).
#
# Argomenti
#   dates, sa_ca   date e serie destagionalizzata e corretta per il calendario
#   holidays       matrice delle festività (una colonna per festa, con i nomi),
#                  es. rjd3toolkit::holidays(..., type = "All"); NULL = Italia.
#                  Se c'è una colonna "Easter Monday" si calcola anche la tabella 6
#   exclude        c(inizio, fine) di un periodo da escludere (es. Covid), o NULL
#   tol            tolleranza attorno a 1 (0,05 = ±5%)
#   extra_dates    date "mm-gg" da controllare, es. c("12-07", "06-24")
# ------------------------------------------------------------------------------
dmr_diagnostics <- function(dates, sa_ca, holidays = NULL, exclude = NULL, tol = 0.05,
                            extra_dates = NULL, digits = 3) {

  stopifnot(length(dates) == length(sa_ca))
  n      <- length(dates)
  wd     <- lubridate::wday(dates, week_start = 1)
  dom    <- as.integer(format(dates, "%d"))
  md     <- format(dates, "%m-%d")
  to_eom <- as.integer(lubridate::days_in_month(dates)) - dom
  rel    <- sa_ca / ave(sa_ca, format(dates, "%Y-%m"), FUN = function(x) median(x, na.rm = TRUE))
  keep   <- rep(TRUE, n)
  if (!is.null(exclude)) keep <- !(dates >= as.Date(exclude[1]) & dates <= as.Date(exclude[2]))

  wd_lab <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  med    <- function(x) round(median(x, na.rm = TRUE), digits)
  n_out  <- 0; n_tot <- 0
  report <- function(title, tab, vals) {
    vals <- as.numeric(vals)
    o <- sum(abs(vals - 1) > tol, na.rm = TRUE); t <- sum(!is.na(vals))
    n_out <<- n_out + o; n_tot <<- n_tot + t
    cat("\n---", title, "---\n"); print(tab)
    cat("Outside +/-", 100 * tol, "%: ", o, " of ", t, "\n", sep = "")
  }

  cat("\n================ DMR DIAGNOSTICS ================\n")
  cat("Days used:", sum(keep), if (!is.null(exclude)) paste("(excluded", exclude[1], "-", exclude[2], ")"), "\n")

  # 1-3. Settimana e mese
  t1 <- tapply(rel[keep], factor(wd[keep], 1:7, wd_lab), med)
  report("1. Day of the week", t1, t1)
  t2 <- tapply(rel[keep], dom[keep], med)
  report("2. Day of the month", t2, t2)
  k3 <- keep & to_eom <= 9
  t3 <- tapply(rel[k3], to_eom[k3], med); names(t3) <- paste0("EoM-", names(t3))
  report("3. Days to month end", t3, t3)

  # Festività: matrice passata dall'utente (qualsiasi paese) oppure, se manca,
  # festività italiane. Deve avere una colonna per festa, con i nomi, allineata
  # alle date (es. rjd3toolkit::holidays(..., type = "All")).
  if (is.null(holidays)) {
    it_cal <- rjd3toolkit::national_calendar(days = list(
      rjd3toolkit::fixed_day(4, 25), rjd3toolkit::fixed_day(6, 2), rjd3toolkit::fixed_day(12, 8),
      rjd3toolkit::fixed_day(12, 26), rjd3toolkit::fixed_day(1, 6),
      rjd3toolkit::special_day('NEWYEAR'), rjd3toolkit::special_day('EASTERMONDAY'),
      rjd3toolkit::special_day('MAYDAY'), rjd3toolkit::special_day('ASSUMPTION'),
      rjd3toolkit::special_day('ALLSAINTSDAY'), rjd3toolkit::special_day('CHRISTMAS')))
    holidays <- rjd3toolkit::holidays(calendar = it_cal, start = min(dates), length = n,
                                      type = "All", nonworking = c(6, 7))
    colnames(holidays) <- c("Liberation Day", "Republic Day", "Immaculate Conception",
                            "St. Stephen's Day", "Epiphany", "New Year's Day", "Easter Monday",
                            "Labour Day", "Assumption of Mary", "All Saints' Day", "Christmas")
  }
  hol <- as.matrix(holidays)
  stopifnot(nrow(hol) == n, !is.null(colnames(hol)))
  # La domenica di Pasqua cade sempre di domenica: esclusa dalle tabelle 4-5
  hol <- hol[, !grepl("^Easter( Sunday)?$", colnames(hol)), drop = FALSE]
  hol_names <- colnames(hol)
  em_col <- grep("Easter Monday", hol_names)
  em_idx <- if (length(em_col)) which(hol[, em_col[1]] == 1) else integer(0)
  xmas_win <- md >= "12-20" | md <= "01-10"

  # 4. Attorno alle festività, per giorno della settimana della festa
  #    (escluse Pasquetta, nella tabella 6, e il periodo natalizio, nella 7)
  h_idx <- which(rowSums(hol[, setdiff(seq_len(ncol(hol)), em_col), drop = FALSE]) > 0 & !xmas_win)
  t4 <- sapply(-3:3, function(k) {
    idx <- h_idx + k; ok <- idx >= 1 & idx <= n & keep[pmin(pmax(idx, 1), n)]
    tapply(rel[idx[ok]], factor(wd[h_idx[ok]], 1:7, wd_lab), med)
  })
  colnames(t4) <- paste0("k", -3:3)
  n_h <- as.integer(table(factor(wd[h_idx[keep[h_idx]]], 1:7, wd_lab)))
  report("4. Around holidays, by weekday of the holiday", cbind(t4, n = n_h), t4)

  # 5. Il giorno della festa, in settimana e nel weekend
  t5 <- t(sapply(seq_len(ncol(hol)), function(j) {
    idx <- which(hol[, j] == 1 & keep)
    c(weekday = med(rel[idx][wd[idx] <= 5]), weekend = med(rel[idx][wd[idx] > 5]),
      n_weekday = sum(wd[idx] <= 5), n_weekend = sum(wd[idx] > 5))
  }))
  rownames(t5) <- hol_names
  report("5. Holiday itself, weekday vs weekend", t5, t5[, 1:2])

  # 6. Settimana di Pasqua (k = 0 domenica di Pasqua)
  if (length(em_idx)) {
    es_idx <- em_idx - 1
    t6 <- sapply(-4:4, function(k) { idx <- es_idx + k; idx <- idx[idx >= 1 & idx <= n]; med(rel[idx][keep[idx]]) })
    names(t6) <- c("Wed", "HolyThu", "GoodFri", "HolySat", "Easter", "EasterMon", "Tue", "Wed", "Thu")
    report("6. Easter week", t6, t6)
  }

  # 7. Periodo natalizio
  xl <- c(sprintf("12-%02d", 15:31), sprintf("01-%02d", 1:10))
  k7 <- keep & md %in% xl
  t7 <- tapply(rel[k7], factor(md[k7], xl), med)
  report("7. Christmas period", t7, t7)

  # 8. Agosto
  k8 <- keep & format(dates, "%m") == "08"
  t8 <- tapply(rel[k8], dom[k8], med)
  report("8. August", t8, t8)

  # 9. Date scelte
  if (!is.null(extra_dates)) {
    t9 <- t(sapply(extra_dates, function(d) {
      idx <- which(md == d & keep)
      c(weekday = med(rel[idx][wd[idx] <= 5]), weekend = med(rel[idx][wd[idx] > 5]),
        n_weekday = sum(wd[idx] <= 5), n_weekend = sum(wd[idx] > 5))
    }))
    report("9. Selected dates", t9, t9[, 1:2])
  }

  cat("\nTOTAL groups outside +/-", 100 * tol, "%: ", n_out, " of ", n_tot, "\n", sep = "")
  cat("==================================================\n")
  invisible(list(dmr = rel, keep = keep))
}
