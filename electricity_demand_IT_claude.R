################################################################################
#                                                                              #
#   DESTAGIONALIZZAZIONE GIORNALIERA DEL FABBISOGNO ELETTRICO (ITALIA)         #
#   Versione finale: effetti di calendario VARIABILI NEL TEMPO                 #
#                                                                              #
#   Commenti in italiano; nomi di regressori, tabelle e grafici in inglese     #
#   (per la presentazione a un pubblico internazionale).                       #
#                                                                              #
################################################################################
#
# OBIETTIVO
# ---------
# Destagionalizzare la serie giornaliera del fabbisogno elettrico nazionale
# (MWh, 2012-2025), proxy ad alta frequenza della produzione industriale, con
# particolare attenzione alla costruzione del calendario: quali giorni
# "speciali" esistono, quanto pesano e come cambiano nel tempo.
#
# SCELTE FINALI
# -------------
# - Modello ADDITIVO (scelto dal test AICc log/livelli): effetti in MWh.
# - Calendario di 23 regressori (vedi SPECIFICAZIONE).
# - Effetti di calendario VARIABILI NEL TEMPO (coefficienti a passeggiata
#   aleatoria, modello rjd3sts), stimati prima del pre-trattamento
#   (approccio in due passi, joint_calendar = FALSE).
# - Decomposizione AMB sequenziale: periodo 7, poi 365.2425.
#
# ------------------------------------------------------------------------------
# PROCEDURA PASSO PER PASSO (i numeri corrispondono alle sezioni del codice)
# ------------------------------------------------------------------------------
# PASSO 0  Librerie, percorsi, interruttori.
# PASSO 1  Importazione e restrizione del periodo.
# PASSO 2  Esplorazione del grezzo: serie, boxplot, periodogramma, KDE.
# PASSO 3  Test log/livelli (AICc) -> modello additivo.
# PASSO 4  Costruzione del calendario:
#          4a festività nazionali, azzerate nel weekend (Pasqua a parte);
#          4b ponti e giorni di semi-chiusura (solo feriali);
#          4c festività di sabato e finestra di Pasqua;
#          4d effetto aggiuntivo delle festività infrasettimanali;
#          4e controlli automatici sulla matrice dei regressori.
# PASSO 5  Stima degli effetti di calendario variabili nel tempo (rjd3sts):
#          coefficienti per anno, grafici dei percorsi, serie corretta.
# PASSO 6  Pre-trattamento: outlier (AO, WO, LS) sulla serie corretta.
# PASSO 7  Decomposizione AMB sequenziale (settimanale, poi annuale).
# PASSO 8  Diagnostiche standard: test di stagionalità, periodogramma,
#          irregolare, KDE.
# PASSO 9  Confronto con la destagionalizzazione mensile.
# PASSO 10 Output giornalieri e mensili.
# PASSO 11 Diagnostica del calendario (il cuore del lavoro).
# PASSO 12 Esportazione in Excel e salvataggio del workspace.
#
# ------------------------------------------------------------------------------
# SPECIFICAZIONE DEI REGRESSORI DI CALENDARIO (23)
# ------------------------------------------------------------------------------
# Nome (inglese)            Significato
# Liberation Day ...        11 festività nazionali, attive solo lun-ven
#   ... Christmas
# Easter Sunday             Pasqua (sempre di domenica), rispetto a una
#                           domenica normale; ricavata dal Lunedì dell'Angelo
# Holiday_Sat               festività cadute di sabato (un unico regressore)
# Holiday_Midweek           effetto aggiuntivo per festività di mar/mer/gio
# Good_Friday               Venerdì Santo
# Holy_Saturday             Sabato Santo
# Easter_Tuesday            martedì dopo il Lunedì dell'Angelo
# Bridge_Mon                lunedì prima di una festività di martedì (ponte)
# Bridge_Fri                venerdì dopo una festività di giovedì (ponte)
# Christmas_Eve_Wkd         24 dicembre, se feriale
# New_Years_Eve_Wkd         31 dicembre, se feriale
# Assumption_Adjacent_Wkd   14 e 16 agosto, se feriali
# Christmas_Period_Wkd      27-30 dicembre e 2-5 gennaio, se feriali e non festivi
# Nessun regressore intra-mensile (fine mese, giorno del mese): i profili
# diagnostici sono piatti, quindi non servono.
#
# ------------------------------------------------------------------------------
# STORIA DELLE SCELTE (in ordine cronologico)
# ------------------------------------------------------------------------------
# a) Diagnostica trasferita dalla fatturazione, calendario no.
#    Profili per giorno della settimana, giorno del mese e distanza dalla fine
#    del mese piatti (0.996-1.004): nessun effetto intra-mensile.
#
# b) Variabilità nel tempo: prima un artefatto, poi reale.
#    - Modello originale (festività attive anche nel weekend): escursione dei
#      coefficienti 135-190 mila MWh. Azzerando le festività nel weekend è
#      crollata a 17-42 mila: gran parte era un artefatto (es. Ognissanti 2025
#      di sabato: coefficiente a fine campione -54 mila invece di -154 mila).
#    - Con il calendario COMPLETO la variazione residua è risultata REALE:
#      i coefficienti delle festività si riducono in modo graduale e comune,
#      soprattutto dal 2018 (circa -10/-18% tra 2012 e 2025). Da qui la scelta
#      finale dei coefficienti variabili.
#
# c) Festività nel weekend: azzerate (trattate come un weekend normale);
#    Pasqua ricavata dal Lunedì dell'Angelo. Il sabato festivo resta però più
#    basso di un sabato normale: regressore Holiday_Sat.
#
# d) Ponti e giorni di semi-chiusura: lunedì prima di una festività di martedì
#    -10%, venerdì dopo una festività di giovedì -9%, vigilia e San Silvestro
#    feriali -6/-7%. Hanno data fissa ma la stagionalità annuale non li cattura:
#    l'effetto esiste solo nei feriali e la componente annuale ne assorbe solo
#    la media (feriali corretti troppo poco, weekend corretti artificialmente).
#
# e) Finestra di Pasqua: il calo va da Venerdì Santo al martedì dopo
#    Pasquetta. Modellando i giorni vicini migliorano anche Pasqua e Pasquetta,
#    che avevano già il loro regressore: un calo su più giorni non modellato
#    viene assorbito dal livello locale e fa sottostimare anche gli effetti dei
#    giorni modellati.
#
# f) Festività infrasettimanali: nel modello additivo una festività di
#    mercoledì parte da un livello più alto di una di lunedì (mar-gio sono i
#    giorni di massimo consumo): effetto aggiuntivo Holiday_Midweek.
#
# g) Confronto finale tra coefficienti fissi (stima congiunta) e variabili
#    (due passi), a parità di calendario:
#                                        fissi            variabili
#      festività mar/mer (giorno stesso) 0.965 / 0.960    0.977 / 0.977
#      ponti lunedì / venerdì            0.993 / 0.987    1.000 / 0.993
#      Pasqua / Pasquetta / martedì      0.995/0.990/0.988 0.983/0.978/0.977
#      periodo natalizio, agosto         simili           simili
#    Le due serie SA_CA differiscono di meno dello 0.2% nella gran parte dei
#    giorni; le differenze maggiori (3-4%) sono sulle festività del 2024-2025,
#    dove i coefficienti variabili usano valori attuali e non la media dei
#    14 anni. Scelta: coefficienti variabili, coerenti con la deriva osservata.
#
# h) Correzioni tecniche rispetto allo script originale:
#    - nel ramo log del modello rjd3sts stati e componenti venivano
#      esponenziati e poi trattati come se fossero in log (non attivo qui,
#      modello additivo);
#    - in log la SA si ottiene moltiplicando per il fattore di calendario;
#    - rjd3filters caricato per primo (copriva dplyr::filter);
#    - legenda DOW/DOY corretta.
#
# ------------------------------------------------------------------------------
# EVOLUZIONE DEI COEFFICIENTI (migliaia di MWh, fine 2012 -> fine 2025)
# ------------------------------------------------------------------------------
#   Republic Day            -174 -> -143      Easter Monday      -221 -> -198
#   Immaculate Conception   -173 -> -153      New Year's Day     -188 -> -172
#   Liberation Day          -168 -> -151      Christmas          -187 -> -176
#   Labour Day              -170 -> -154      All Saints' Day    -170 -> -157
#   Epiphany                -165 -> -151      St. Stephen's Day  -177 -> -165
#   Assumption of Mary      -132 -> -128      Easter Sunday       -59 ->  -57
#   Bridge_Mon               -52 ->  -77      New_Years_Eve_Wkd   -76 ->  -97
#   Holiday_Sat              -73 ->  -55      Easter_Tuesday      -30 ->  -46
#   Holiday_Midweek          -21 ->   +2 (*)
# (*) Holiday_Midweek si sovrappone ai regressori delle singole festività:
#     i due percorsi possono compensarsi e presi singolarmente sono poco
#     affidabili. Conta la somma (per una festività di mercoledì: festività +
#     effetto infrasettimanale).
# Nota: il modello usa un'unica varianza per tutti i regressori.
#
# ------------------------------------------------------------------------------
# OUTLIER (15, tutti eventi)
# ------------------------------------------------------------------------------
# - Covid: LS 15/03/2020 (-132 mila MWh, lockdown); LS 25/04/2020 e 27/07/2020
#   (+68 e +67 mila, riaperture).
# - Crisi energetica: LS 22/08/2022 (-42 mila).
# - Temperatura (ondate di calore ed estati fresche): LS 15/06/2014 e
#   01/09/2014; 29/08/2016; 10/07/2023, 26/07/2023, 19/08/2023; 09/07/2024 e
#   10/09/2024; 04/06/2025 e 08/07/2025. Coppie di cambi di livello estivi di
#   segno opposto: effetti climatici non modellati.
# - AO 21/08/2015 (-71 mila): da verificare.
#
# ------------------------------------------------------------------------------
# DIAGNOSTICA FINALE (indicatore: SA_CA / mediana della SA_CA del mese, ~1)
# ------------------------------------------------------------------------------
# - Giorno della settimana, giorno del mese, fine mese: 0.996-1.00.
# - Festività feriali (giorno stesso): 0.974-0.987; sabato 1.01; domenica 0.993.
# - Ponti: 1.00 e 0.993. Holiday_Sat: 1.00.
# - Finestra di Pasqua: 0.977-0.994.
# - Periodo natalizio: 0.974-1.01. Agosto: 0.979-1.01.
# Limiti noti:
# - finestra di Pasqua corretta un po' meno che con i coefficienti fissi: nel
#   modello in due passi il calendario è stimato con un airline solo
#   settimanale, il cui livello locale assorbe parte dei cali su più giorni;
# - pochi casi, residui minori: vigilia e San Silvestro feriali (0.96-0.98,
#   8 casi ciascuno), 14/16 agosto feriali (0.95, 16 casi).
#
# ------------------------------------------------------------------------------
# PUNTI APERTI
# ------------------------------------------------------------------------------
# - Temperatura (gradi-giorno di riscaldamento e raffrescamento).
# - Ultimi dati del campione arrotondati alle migliaia di MWh: probabilmente
#   provvisori, soggetti a revisione.
# - Cause della deriva degli effetti delle festività (ipotesi: minor peso
#   dell'industria sui consumi, autoproduzione da fotovoltaico).
#
# ------------------------------------------------------------------------------
# LEZIONI PER LE GOOD PRACTICE (dal fabbisogno elettrico)
# ------------------------------------------------------------------------------
# 1. La diagnostica si trasferisce, il calendario no: i regressori vanno
#    costruiti a partire dall'evidenza di ciascuna serie.
# 2. Coefficienti liberi di variare assorbono gli errori di specificazione e
#    mostrano una variabilità che non c'è: prima si corregge il calendario.
# 3. Ma la variabilità nel tempo va verificata con il calendario COMPLETO,
#    non a metà strada: togliere gli errori non basta a escluderla.
# 4. Un effetto a data fissa non è per forza stagionale: se dipende dal
#    giorno della settimana, la componente annuale ne cattura solo la media.
# 5. Modellare anche i giorni vicini alle festività: un calo su più giorni
#    non modellato fa sottostimare anche gli effetti dei giorni modellati.
# 6. L'effetto di una festività dipende dal livello del giorno in cui cade
#    (sabato festivo != domenica; mercoledì festivo != lunedì festivo).
# 7. Gli outlier indicano cosa manca: coppie di cambi di livello estivi
#    segnalano effetti climatici non modellati.
#
################################################################################


################################################################################
#  PASSO 0 - LIBRERIE, PERCORSI, INTERRUTTORI                                  #
################################################################################

# rjd3filters caricato PER PRIMO: la sua funzione filter() coprirebbe dplyr::filter.
# Usare comunque sempre dplyr::filter in modo esplicito.
library(rjd3filters)
library(rjd3toolkit)
library(rjd3highfreq)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)
library(lmtest)
library(urca)
library(aTSA)
library(rJava)
library(readxl)
library(openxlsx)
library(rjd3sts)
library(rjd3x11plus)
library(rjd3stl)

base_path                 <- "C:\\Users\\alessandro.piovani\\Desktop\\github_repo\\NTTS_high_freq_daily"
# base_path                 <- "C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\NTTS_high_freq_daily"
input_data_path           <- paste0(base_path,"\\Fabbisogno_elettrico_e_prod_industriale\\Electricity_Fabbisogno_Totale_Italia.csv")
monthly_SA_benchmark_path <- paste0(base_path,"\\Fabbisogno_elettrico_e_prod_industriale\\sa_new_IndiciProduzioneIndustriale.csv")
setwd(base_path)

blank_par <- par(no.readonly = TRUE) # impostazioni grafiche di base

# load("workspace_electricity_demand_IT.RData")

source("utilities.R")
source("revision.R")
source("stability.R")

label          <- "Electricity demand IT (MWh)"
label_original <- "Electricity demand IT (MWh)"
lab <- label
date_start <- "2012-01-01"
date_end   <- "2025-12-31"

# ------------------------------------------------------------------------------
# INTERRUTTORI
# Ogni interruttore corrisponde a un passo del percorso di costruzione del
# calendario: spegnendoli si possono riprodurre le versioni intermedie.
# ------------------------------------------------------------------------------
# joint_calendar = FALSE -> SCELTA FINALE: effetti di calendario variabili nel
#                           tempo (rjd3sts), poi pre-trattamento (due passi)
# joint_calendar = TRUE  -> alternativa: calendario stimato dentro il
#                           pre-trattamento, coefficienti fissi
joint_calendar <- FALSE

# Festività azzerate quando cadono di sabato/domenica (Pasqua a parte)
fix_weekend_holidays <- TRUE

# Ponti e giorni di semi-chiusura (Bridge_Mon, Bridge_Fri, Christmas_Eve_Wkd,
# New_Years_Eve_Wkd, Assumption_Adjacent_Wkd, Christmas_Period_Wkd)
add_extra_days <- TRUE

# Festività di sabato e finestra di Pasqua (Holiday_Sat, Good_Friday,
# Holy_Saturday, Easter_Tuesday)
add_easter_sat <- TRUE

# Effetto aggiuntivo delle festività di martedì/mercoledì/giovedì (Holiday_Midweek)
add_midweek <- TRUE


################################################################################
#  PASSO 1 - IMPORTAZIONE E RESTRIZIONE DEL PERIODO                            #
################################################################################

data <- read.csv2(input_data_path)
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

data <- data %>% 
  dplyr::filter(Date >= as.Date(date_start)) %>% 
  dplyr::filter(Date <= as.Date(date_end)) 
data$Date <- as.Date(data$Date)


################################################################################
#  PASSO 2 - ESPLORAZIONE DEL GREZZO                                           #
################################################################################

# 2a) Serie grezza
options(scipen = 999)
plot(data$Date, data$Value, type = "l",
     xlab = "Year", ylab = label_original, axes = TRUE)  
title(main = "Unadjusted data", font.main = 1)

# 2b) Boxplot per giorno della settimana e per mese
Sys.setlocale("LC_TIME", "C")   # nomi di giorni e mesi in inglese

data <- data %>%
  mutate(
    day   = factor(weekdays(Date),
                   levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                              "Friday", "Saturday", "Sunday"), ordered = TRUE),
    month = factor(month.name[month(Date)], levels = month.name, ordered = TRUE),
    year  = factor(year(Date), ordered = TRUE)  
  )

theme_clean <- theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black"),
  plot.background = element_blank(),
  plot.title = element_text(hjust = 0.5)
)

ggplot(data, aes(x = day, y = Value)) +
  geom_boxplot(fill = "grey80", color = "black") +
  labs(x = "Day", y = label_original, title = "Day-of-the-week pattern") +
  theme_clean

ggplot(data, aes(x = month, y = Value)) +
  geom_boxplot(fill = "grey80", color = "black") +
  labs(x = "Month", y = label_original, title = "Day-of-the-year pattern") +
  theme_clean

data <- data %>% select(-day, -month, -year)

# 2c) Periodogramma del grezzo (linee: armoniche settimanali e annuali)
spec_non_seasonal <- spec.pgram(data$Value, log = FALSE, plot = FALSE)
plot(spec_non_seasonal$freq, spec_non_seasonal$spec, type = "l", 
     col = "blue", lwd = 2, xlab = "Frequency", ylab = "Spectral density")
abline(v = c(1/7, 2/7, 3/7), col = "gray", lty = 2)                       # settimanale
abline(v = c(1/365.2425, 2/365.2425, 3/365.2425), col = "gray", lty = 4)  # annuale
title(main = "Raw series - Periodogram", font.main = 1)

# 2d) KDE delle autocorrelazioni mobili ai ritardi 7 e 365
ts_raw_data <- with(data[order(data$Date), ],
                    ts(Value,
                       start = c(as.numeric(format(min(Date), "%Y")),
                                 as.numeric(format(min(Date), "%j"))),
                       frequency = 365))

xlim_range <- c(-0.5, 0.5)
ylim_range <- c(0, 40)

KDE_raw <- plot_kde_acf(raw_data = ts_raw_data, lags = c(7, 365), window_cycles = c(4, 2),
                        xlim_min = xlim_range[1], xlim_max = xlim_range[2],
                        ylim_min = ylim_range[1], ylim_max = ylim_range[2],
                        show_legend = TRUE, title_label = "KDE of rolling ACF for raw data")
KDE_raw


################################################################################
#  PASSO 3 - TEST LOG/LIVELLI (AICc)                                           #
################################################################################
# Confronta un modello additivo e uno moltiplicativo (airline esteso, periodi
# 7 e 365.2425). Su questi dati vince l'ADDITIVO: effetti di calendario in MWh.

data <- data %>% dplyr::mutate(Value = dplyr::if_else(Value <= 0, 1, Value))

jspec     <- rjd3highfreq::.extended_airline_spec(periodicities = c(7, 365.2425),
                                                  differencing = -1, ar = FALSE, toint = FALSE)
jregarima <- rjd3highfreq::.extended_airline_regarima(y = data$Value, jspec = jspec)
aiccs     <- rjd3highfreq::.extended_airline_loglevel(jregarima = jregarima, jspec = jspec)

if (aiccs[1] <= aiccs[2]) {
  print("No log transformation applied")
  log <- FALSE
} else {
  print("Log transformation applied")
  log <- TRUE
}
aiccs[1]
aiccs[2]


################################################################################
#  PASSO 4 - COSTRUZIONE DEL CALENDARIO                                        #
################################################################################

# Giorno della settimana, indipendente dalla lingua del sistema:
# 1 = lunedì ... 6 = sabato, 7 = domenica
wd_h   <- lubridate::wday(data$Date, week_start = 1)
is_wkd <- wd_h <= 5
md_all <- format(data$Date, "%m-%d")
n_obs  <- nrow(data)

# ------------------------------------------------------------------------------
# 4a) Festività nazionali
# ------------------------------------------------------------------------------
italian_calendar <- rjd3toolkit::national_calendar(days = list(
  rjd3toolkit::fixed_day(4, 25),            # Liberation Day - 25 aprile
  rjd3toolkit::fixed_day(6, 2),             # Republic Day - 2 giugno
  rjd3toolkit::fixed_day(12, 8),            # Immaculate Conception - 8 dicembre
  rjd3toolkit::fixed_day(12, 26),           # St. Stephen's Day - 26 dicembre
  rjd3toolkit::fixed_day(1, 6),             # Epiphany - 6 gennaio
  rjd3toolkit::special_day('NEWYEAR'),      # New Year's Day - 1 gennaio
  rjd3toolkit::special_day('EASTERMONDAY'), # Easter Monday - Lunedì dell'Angelo
  rjd3toolkit::special_day('MAYDAY'),       # Labour Day - 1 maggio
  rjd3toolkit::special_day('ASSUMPTION'),   # Assumption of Mary - 15 agosto
  rjd3toolkit::special_day('ALLSAINTSDAY'), # All Saints' Day - 1 novembre
  rjd3toolkit::special_day('CHRISTMAS'),    # Christmas - 25 dicembre
  rjd3toolkit::special_day('EASTER')        # Easter Sunday - Pasqua
))

# Nomi in inglese (ordine = ordine del calendario sopra)
hol_names <- c("Liberation Day", "Republic Day", "Immaculate Conception", "St. Stephen's Day",
               "Epiphany", "New Year's Day", "Easter Monday", "Labour Day",
               "Assumption of Mary", "All Saints' Day", "Christmas", "Easter Sunday")

# Versione "All": festività attive in qualsiasi giorno della settimana.
# Serve come base: le festività nel weekend vengono azzerate a mano più sotto
# (l'opzione type = "Skip" azzererebbe anche Pasqua, che cade sempre di domenica).
cal_all <- rjd3toolkit::holidays(
  calendar = italian_calendar,
  start = min(data$Date), length = n_obs,
  type = "All", nonworking = c(6, 7))

em_vec  <- as.numeric(cal_all[, 7])                         # Lunedì dell'Angelo
hol_any <- rowSums(cal_all[, 1:11, drop = FALSE]) > 0       # festività (esclusa Pasqua), qualsiasi giorno

if (fix_weekend_holidays) {
  cal_reg <- cal_all
  cal_reg[, 1:11] <- cal_all[, 1:11] * is_wkd               # festività solo lun-ven
  cal_reg[, 12]   <- c(em_vec[-1], 0)                       # Pasqua = giorno prima di Pasquetta
  stopifnot(
    sum(em_vec) > 0,
    all(wd_h[em_vec == 1] == 1),                            # Pasquetta sempre di lunedì
    all(wd_h[cal_reg[, 12] == 1] == 7),                     # Pasqua sempre di domenica
    sum(cal_reg[, 12]) == sum(em_vec),
    all(wd_h[rowSums(cal_reg[, 1:11, drop = FALSE]) > 0] <= 5)  # nessuna festività nel weekend
  )
} else {
  cal_reg <- cal_all
}
cal_reg <- as.matrix(cal_reg)
colnames(cal_reg) <- hol_names

# ------------------------------------------------------------------------------
# 4b) Ponti e giorni di semi-chiusura (solo giorni feriali)
# ------------------------------------------------------------------------------
next_hol <- c(hol_any[-1], FALSE)                 # domani è festivo
prev_hol <- c(FALSE, hol_any[-n_obs])             # ieri era festivo
xmas_workdays <- c("12-27", "12-28", "12-29", "12-30", "01-02", "01-03", "01-04", "01-05")

bridge_mon_vec <- as.numeric(wd_h == 1 & next_hol & !hol_any)   # lunedì prima di festività di martedì
bridge_fri_vec <- as.numeric(wd_h == 5 & prev_hol & !hol_any)   # venerdì dopo festività di giovedì

if (add_extra_days) {
  extra_reg <- cbind(
    Bridge_Mon              = bridge_mon_vec,
    Bridge_Fri              = bridge_fri_vec,
    Christmas_Eve_Wkd       = as.numeric(md_all == "12-24" & is_wkd),
    New_Years_Eve_Wkd       = as.numeric(md_all == "12-31" & is_wkd),
    Assumption_Adjacent_Wkd = as.numeric(md_all %in% c("08-14", "08-16") & is_wkd),
    Christmas_Period_Wkd    = as.numeric(md_all %in% xmas_workdays & is_wkd & !hol_any)
  )
  cal_reg <- cbind(cal_reg, extra_reg)
}

# ------------------------------------------------------------------------------
# 4c) Festività di sabato e finestra di Pasqua
# ------------------------------------------------------------------------------
good_friday_vec    <- c(em_vec[-(1:3)], rep(0, 3))   # 3 giorni prima di Pasquetta
holy_saturday_vec  <- c(em_vec[-(1:2)], rep(0, 2))   # 2 giorni prima di Pasquetta
easter_tuesday_vec <- c(0, em_vec[-n_obs])           # giorno dopo Pasquetta
holiday_sat_vec    <- as.numeric(hol_any & wd_h == 6)

if (add_easter_sat) {
  extra3 <- cbind(
    Holiday_Sat    = holiday_sat_vec,
    Good_Friday    = good_friday_vec,
    Holy_Saturday  = holy_saturday_vec,
    Easter_Tuesday = easter_tuesday_vec
  )
  stopifnot(
    all(wd_h[extra3[, "Good_Friday"] == 1] == 5),     # sempre venerdì
    all(wd_h[extra3[, "Holy_Saturday"] == 1] == 6),   # sempre sabato
    all(wd_h[extra3[, "Easter_Tuesday"] == 1] == 2)   # sempre martedì
  )
  cal_reg <- cbind(cal_reg, extra3)
}

# ------------------------------------------------------------------------------
# 4d) Effetto aggiuntivo delle festività infrasettimanali (mar/mer/gio)
# ------------------------------------------------------------------------------
if (add_midweek) {
  Holiday_Midweek <- as.numeric(hol_any & wd_h %in% 2:4)
  stopifnot(sum(Holiday_Midweek) > 0)
  cal_reg <- cbind(cal_reg, Holiday_Midweek = Holiday_Midweek)
}

# ------------------------------------------------------------------------------
# 4e) Controlli automatici sulla matrice dei regressori
# ------------------------------------------------------------------------------
zero_cols <- colnames(cal_reg)[colSums(cal_reg != 0) == 0]
if (length(zero_cols) > 0) stop(paste("Calendar regressors always zero:", paste(zero_cols, collapse = ", ")))
if (qr(cal_reg)$rank < ncol(cal_reg)) stop("Calendar regressor matrix is not full rank")

print("Calendar regressors - number of active days:")
print(colSums(cal_reg != 0))


################################################################################
#  PASSO 5 - EFFETTI DI CALENDARIO VARIABILI NEL TEMPO (rjd3sts)               #
################################################################################
# Modello: airline settimanale + regressori di calendario con coefficienti a
# passeggiata aleatoria (un'unica varianza per tutti, stimata).
# Stati e componenti restano nella scala del modello (log o livelli): NON vanno
# esponenziati qui (errore presente nello script originale).

if (!joint_calendar) {
  
  sm2 <- model()
  eq2 <- equation("eq")
  add(sm2, sarima("airline", c(7), c(0, 1, 1), c(0, 1, 1)))
  add(sm2, reg("cal", cal_reg, 0.1))
  
  y_sts <- if (log) log(data$Value) else data$Value
  rslt2 <- rjd3sts::estimate(sm2, y_sts,
                             marginal = F,
                             initialization = "SqrtDiffuse",
                             optimizer = "LevenbergMarquardt", 
                             concentrated = T, 
                             precision = 1e-10)
  
  fs  <- result(rslt2, "ssf.filtered.states")
  ss  <- result(rslt2, "ssf.smoothing.states")
  cmp <- result(rslt2, "ssf.smoothing.components")
  
  # Colonne di ss relative ai regressori: le prime 9 sono gli stati dell'airline settimanale
  cal_idx <- 10:(9 + ncol(cal_reg))
  
  # 5a) Escursione di ciascun coefficiente sul campione (max - min)
  coef_range <- apply(ss[, cal_idx], 2, function(x) diff(range(x)))
  names(coef_range) <- colnames(cal_reg)
  print("Range of time-varying calendar coefficients over the sample:")
  print(round(coef_range))
  
  # 5b) Coefficienti a fine anno: tabella regressori x anni.
  #     Modello additivo: migliaia di MWh. Modello log: fattori moltiplicativi.
  idx_ye <- tapply(seq_len(n_obs), year(data$Date), max)
  coef_by_year <- ss[as.integer(idx_ye), cal_idx, drop = FALSE]
  coef_by_year <- if (log) round(exp(coef_by_year), 3) else round(coef_by_year / 1000)
  coef_by_year <- t(coef_by_year)
  rownames(coef_by_year) <- colnames(cal_reg)
  colnames(coef_by_year) <- names(idx_ye)
  print(if (log) "Calendar coefficients at year end (multiplicative factors):"
        else "Calendar coefficients at year end (thousand MWh):")
  print(coef_by_year)
  
  # 5c) Grafici dei percorsi dei coefficienti: festività e altri regressori separati
  plot_coef_paths <- function(idx_sel, main_title) {
    cols <- hcl.colors(length(idx_sel), "Dark 3")
    y_vals <- ss[, cal_idx[idx_sel], drop = FALSE]
    if (!log) y_vals <- y_vals / 1000
    par(blank_par)
    layout(matrix(c(1, 2), nrow = 2), heights = c(4, 1.6))
    par(mar = c(5, 4, 4, 2) + 0.1)
    plot(data$Date, y_vals[, 1], type = "l", col = cols[1], lwd = 1.5,
         ylim = range(y_vals, na.rm = TRUE), xlab = "Time",
         ylab = if (log) "Calendar effect (log scale)" else "Calendar effect (thousand MWh)")
    if (length(idx_sel) > 1) for (k in 2:length(idx_sel)) lines(data$Date, y_vals[, k], col = cols[k], lwd = 1.5)
    abline(h = 0, col = "grey70", lty = 3)
    mtext(main_title, side = 3, line = 1.5, font = 1, cex = 1.2)
    par(mar = c(0, 0, 0, 2))
    plot.new()
    legend("top", inset = 0, legend = colnames(cal_reg)[idx_sel],
           col = cols, lty = 1, lwd = 1.5, ncol = 3, cex = 0.8, bty = "n")
    layout(matrix(1))
    par(blank_par)
  }
  
  plot_coef_paths(1:12, "Time-varying effects: national holidays")
  if (ncol(cal_reg) > 12) {
    plot_coef_paths(13:ncol(cal_reg), "Time-varying effects: bridges, Easter window and special days")
  }
  
  # 5d) Serie corretta per il calendario e effetto di calendario
  #     (il nome Time_Varying è mantenuto per compatibilità con utilities.R)
  if (log) {  
    data$Value_td_adjusted <- exp(log(data$Value) - cmp[, 2])
    data$Time_Varying      <- exp(cmp[, 2])
  } else {
    data$Value_td_adjusted <- data$Value - cmp[, 2]
    data$Time_Varying      <- cmp[, 2]
  }
}


################################################################################
#  PASSO 6 - PRE-TRATTAMENTO: OUTLIER                                          #
################################################################################
# Scelta finale (joint_calendar = FALSE): pre-trattamento sulla serie già
# corretta per il calendario, solo outlier.
# Alternativa (joint_calendar = TRUE): regressori stimati qui, coefficienti fissi.

if (joint_calendar) {
  
  pre_pro <- fractionalAirlineEstimation(
    y = data$Value, x = cal_reg, log = log, criticalValue = 6,
    periods = c(7, 365.2425), outliers = c("ao", "wo", "ls"), y_time = data$Date)
  
  # Estrazione dell'effetto di calendario (log: fattore, neutro 1; livelli: additivo, neutro 0)
  extract_calendar_effect <- function(pp, X, log) {
    n <- nrow(X); k <- ncol(X)
    comp <- pp$model$component_userdef_reg_variables
    if (!is.null(comp) && length(comp) == n) {
      comp <- as.numeric(comp)
      if (log) {
        if (abs(median(comp) - 1) < abs(median(comp))) return(list(eff = comp, how = "component_userdef_reg_variables (factor)"))
        return(list(eff = exp(comp), how = "component_userdef_reg_variables (log -> exp)"))
      }
      return(list(eff = comp, how = "component_userdef_reg_variables"))
    }
    b <- pp$model$b
    if (!is.null(b) && length(b) >= k) {
      e <- as.numeric(X %*% as.numeric(b[1:k]))
      return(list(eff = if (log) exp(e) else e, how = "X %*% b"))
    }
    warning("Calendar effect could not be extracted: SA column not valid")
    list(eff = rep(if (log) 1 else 0, n), how = "NOT EXTRACTED")
  }
  
  cal_eff <- extract_calendar_effect(pre_pro, cal_reg, log)
  data$Time_Varying      <- cal_eff$eff
  data$Value_td_adjusted <- if (log) data$Value / data$Time_Varying else data$Value - data$Time_Varying
  
  # Verifica: osservata - linearizzata - calendario = soli outlier
  remainder <- if (log) log(data$Value) - pre_pro$model$linearized - log(data$Time_Varying)
  else data$Value - pre_pro$model$linearized - data$Time_Varying
  tol <- if (log) 1e-6 else 1e-6 * median(abs(data$Value))
  share_ok <- mean(abs(diff(remainder)) < tol)
  
} else {
  
  pre_pro <- fractionalAirlineEstimation(
    y = data$Value_td_adjusted, log = log, criticalValue = 6,
    periods = c(7, 365.2425), outliers = c("ao", "wo", "ls"), y_time = data$Date) 
}

print(pre_pro)

par(blank_par)
plot(pre_pro, main = label_original)

ls_presents <- (!is.null(pre_pro$model$component_ls) && 
                  length(pre_pro$model$component_ls) > 0 && 
                  !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls)))


################################################################################
#  PASSO 7 - DECOMPOSIZIONE AMB SEQUENZIALE                                    #
################################################################################

lin_series <- pre_pro$model$linearized

# 7a) Stagionalità settimanale (periodo 7)
amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
  y = lin_series, period = 7, y_time = data$Date) 

if (log && !grepl("\\(log-scale\\)", label)) label <- paste0(label_original, " (log-scale)")

options(scipen = 999)
print(amb.dow)
par(blank_par)
plot(amb.dow, main = "Sequential approach, s = 7")
mtext(label, side = 2, line = 2.5)

# 7b) Stagionalità annuale (periodo 365.2425) sulla serie corretta per la settimanale
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
  y = amb.dow$decomposition$sa, period = 365.2425, y_time = data$Date)

# 7c) Reinserimento dei cambi di livello (LS) nella SA
if (ls_presents) {
  if (log) {
    # component_ls è un rapporto in scala di livello (neutro = 1): in log si somma il suo log
    amb.doy$decomposition$sa <- amb.doy$decomposition$sa + log(pre_pro$model$component_ls)
  } else {
    amb.doy$decomposition$sa <- amb.doy$decomposition$sa + pre_pro$model$component_ls
  }
}

par(blank_par)
plot(amb.doy, main = "Sequential approach, s = 365.2425")
mtext(label, side = 2, line = 2.5)
print(amb.doy)

if (log) amb.doy$decomposition$sa <- exp(amb.doy$decomposition$sa)

# 7d) Grezzo vs SA
par(blank_par)
layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
par(mar = c(5, 4, 4, 2))
plot(amb.dow$decomposition$y_time, data$Value, type = "l", col = "#F1C232",
     xlab = "Time", ylab = "")
lines(amb.dow$decomposition$y_time, amb.doy$decomposition$sa, col = "#006400")
title(main = "Sequential AMB", font.main = 1)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("Raw", "SA"), col = c("#F1C232", "#006400"),
       lty = 1, bty = "n", cex = 1.1)
par(blank_par)

# 7e) Componenti stagionali settimanale e annuale
y_vals <- c(amb.dow$decomposition$s, amb.doy$decomposition$s)
margin <- 0.025 * (max(y_vals) - min(y_vals))  
par(blank_par)
layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
par(mar = c(5, 4, 4, 2))
plot(amb.dow$decomposition$y_time, amb.dow$decomposition$s, type = "l", col = "#FC8D62",
     xlab = "Time", ylab = "", ylim = c(min(y_vals) - margin, max(y_vals) + margin))
lines(amb.dow$decomposition$y_time, amb.doy$decomposition$s, col = "blue")
title(ylab = label)
title(main = "Sequential AMB", font.main = 1)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("DOW", "DOY"), col = c("#FC8D62", "blue"),   # colori corretti
       lty = 1, bty = "n", cex = 1.1)
par(blank_par)


################################################################################
#  PASSO 8 - DIAGNOSTICHE STANDARD                                             #
################################################################################

# 8a) Rapporti S-I per la stagionalità settimanale
out_amb_seq <- compute_si_daily(log = FALSE, s7 = amb.dow$decomposition$s,
                                i = amb.doy$decomposition$i, dates = data$Date)
si_amb_seq  <- plot_si_weekly(dates = out_amb_seq$daily$date, si = out_amb_seq$daily$si,
                              s7 = amb.dow$decomposition$s, log_ratio = FALSE, show = "points")

# 8b) Test di stagionalità: grezzo...
seasonality_f(data$Value, period = 7)
seasonality_f(data$Value, period = 365.2425)
seasonality_kruskalwallis(data$Value, period = 7)
seasonality_kruskalwallis(data$Value, period = 365.2425)
seasonality_periodogram(data$Value, period = 365.2425)
seasonality_periodogram(data$Value, period = 7)
seasonality_qs(data$Value, period = 7)
seasonality_qs(data$Value, period = 365.2425)

# ... e serie destagionalizzata (non deve restare stagionalità)
seasonality_f(amb.doy$decomposition$sa, period = 7)
seasonality_f(amb.doy$decomposition$sa, period = 365.2425)
seasonality_kruskalwallis(amb.doy$decomposition$sa, period = 7)
seasonality_kruskalwallis(amb.doy$decomposition$sa, period = 365.2425)
seasonality_periodogram(amb.doy$decomposition$sa, period = 365.2425)
seasonality_periodogram(amb.doy$decomposition$sa, period = 7)
# Soglie QS: p.9 = 2.49, p.95 = 3.83, p.99 = 7.06, p.999 = 11.88
seasonality_qs(amb.doy$decomposition$sa, period = 7)
seasonality_qs(amb.doy$decomposition$sa, period = 365.2425)

# 8c) Periodogramma: grezzo vs SA
par(blank_par)
layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
par(mar = c(5, 4, 4, 2))
periodogram_raw_data <- spec.pgram(data$Value, log = FALSE, plot = FALSE) 
periodogram_ambSeq   <- spec.pgram(amb.doy$decomposition$sa, log = FALSE, plot = FALSE)
plot(periodogram_raw_data$freq, periodogram_raw_data$spec, type = "l", 
     col = "blue", lwd = 2, xlab = "Frequency", ylab = "Spectral density")
lines(periodogram_ambSeq$freq, periodogram_ambSeq$spec, col = "red", lwd = 2)
abline(v = c(1/7, 2/7, 3/7), col = "gray", lty = 2)
abline(v = c(1/365.2425, 2/365.2425, 3/365.2425), col = "gray", lty = 4)
title(main = "Sequential AMB", font.main = 1)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("Raw", "SA"), col = c("blue", "red"),
       lty = 1, lwd = 2, bty = "n", cex = 1.1)
par(blank_par)

# 8d) Componente irregolare
results_ambSeq_irr <- analyze_irregular(amb.doy$decomposition$i, name = "AMB Sequential")
plot(results_ambSeq_irr$acf)
plot(results_ambSeq_irr$pacf)

standardize <- function(x) {
  x <- x[!is.na(x)]
  (x - mean(x)) / sd(x)
}
irr_ambSeq_std <- standardize(amb.doy$decomposition$i)
plot_hist_panel(amb.doy$decomposition$i, irr_ambSeq_std, "AMB seq")
par(blank_par)

# 8e) KDE delle autocorrelazioni mobili: grezzo vs SA
ts_sa_AMB_seq <- ts(amb.doy$decomposition$sa,
                    start = c(as.numeric(format(min(amb.doy$decomposition$y_time), "%Y")),
                              as.numeric(format(min(amb.doy$decomposition$y_time), "%j"))),
                    frequency = 365)

KDE_AMB_seq <- plot_kde_acf(raw_data = ts_raw_data, sa_data = ts_sa_AMB_seq, lags = c(7, 365),
                            window_cycles = c(4, 2),
                            xlim_min = xlim_range[1], xlim_max = xlim_range[2],
                            ylim_min = ylim_range[1], ylim_max = ylim_range[2],
                            show_legend = TRUE, title_label = "KDE of rolling ACF for AMB sequential")
KDE_AMB_seq

# Analisi delle revisioni (da attivare se necessario)
# amb_estimator2 <- AMBEstimator$new(data = data, dates_from_pre_pro = date_start, log_pre_pro = log)
# results_amb2 <- calculate_revisions_by_lags(estimator_instance = amb_estimator2)


################################################################################
#  PASSO 9 - CONFRONTO CON LA DESTAGIONALIZZAZIONE MENSILE                     #
################################################################################

year_start   <- "2020-01-01"
year_end     <- "2020-12-31"
sa_end_extra <- "2021-01-31"

Sys.setlocale("LC_TIME", "C")

res     <- aggregate_daily_to_monthly(data, fun = mean)
SA_full <- sa_all_methods(res)
SA      <- SA_full %>% filter_range("date", year_start, sa_end_extra)
data2   <- filter_range(data, "Date", year_start, year_end)

plot_sa_ca_comparison(
  sa_vector    = amb.doy$decomposition$sa,
  sa_column    = "TRAMO_SEATS",
  title        = "Sequential AMB",
  legend_label = "SA monthly (TRAMO-SEATS)",
  log = log,
  series = "SA" 
)


################################################################################
#  PASSO 10 - OUTPUT GIORNALIERI E MENSILI                                     #
################################################################################

sa_outputs <- produce_outputs(
  data = data,
  sa_daily = amb.doy$decomposition$sa,
  calendar_effect = data$Time_Varying
)

# SA_CA: destagionalizzata e corretta per il calendario.
# SA:    destagionalizzata, con l'effetto di calendario riapplicato
#        (in log va moltiplicato, in livelli sommato).
daily <- data.frame(
  Date  = data$Date,
  Raw   = data$Value,
  SA_CA = amb.doy$decomposition$sa,
  SA    = if (log) amb.doy$decomposition$sa * data$Time_Varying else amb.doy$decomposition$sa + data$Time_Varying
)

raw_monthly <- daily %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>% summarise(Raw_monthly = mean(Raw, na.rm = TRUE))

sa_ca_monthly <- daily %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>% summarise(SA_CA_monthly = mean(SA_CA, na.rm = TRUE))

sa_monthly <- daily %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>% summarise(SA_monthly = mean(SA, na.rm = TRUE))

rsa_monthly <- data.frame(
  Date = seq(from = floor_date(min(data$Date), "month"), by = "month",
             length.out = length(SA_full$TRAMO_SEATS)),
  RSAfull_TRAMO_SEATS = as.numeric(SA_full$TRAMO_SEATS)
)

monthly <- raw_monthly %>%
  left_join(sa_monthly, by = "month") %>%
  left_join(sa_ca_monthly, by = "month") %>%
  left_join(rsa_monthly, by = c("month" = "Date")) %>%
  rename(Date = month)


################################################################################
#  PASSO 11 - DIAGNOSTICA DEL CALENDARIO                                       #
################################################################################
# Indicatore: rel = SA_CA del giorno / mediana della SA_CA del suo mese.
# Se il calendario è corretto, rel ~ 1 in tutti i gruppi di giorni.
# Si usano le mediane, robuste a outlier e mesi eccezionali.

diag_prof <- daily %>%
  mutate(m      = floor_date(Date, "month"),
         dom    = day(Date),
         to_eom = as.integer(days_in_month(Date)) - day(Date),
         wd     = wday(Date, week_start = 1)) %>%
  group_by(m) %>%
  mutate(rel = SA_CA / median(SA_CA)) %>%
  ungroup()

cat("\n================ DIAGNOSTICS ================\n")
cat("Log transformation:", log, "\n")
cat("Calendar coefficients:", if (joint_calendar) "fixed (joint estimation)" else "time-varying (two-step)", "\n")
cat("Mean SA_CA / mean raw:", round(mean(daily$SA_CA) / mean(daily$Raw), 3), "(expected ~1)\n")
if (joint_calendar) {
  cat("Calendar effect extracted from:", cal_eff$how, "\n")
  cat("Consistent days:", round(100 * share_ok, 1), "% (expected > 95%)\n")
}
cat("=============================================\n\n")

wd_labels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

# 11a) Giorno della settimana: la stagionalità settimanale deve essere sparita
prof_dow <- diag_prof %>%
  mutate(weekday = factor(wd, levels = 1:7, labels = wd_labels)) %>%
  group_by(weekday) %>% summarise(median = round(median(rel), 3), n = n())
print(prof_dow)

# 11b) Giorno del mese: atteso piatto (nessun effetto intra-mensile)
prof_dom <- diag_prof %>%
  group_by(day_of_month = dom) %>% summarise(median = round(median(rel), 3))
print(prof_dom, n = 31)

# 11c) Giorni alla fine del mese (0 = ultimo giorno): atteso piatto
prof_eom <- diag_prof %>% dplyr::filter(to_eom <= 9) %>%
  group_by(days_to_month_end = to_eom) %>% summarise(median = round(median(rel), 3))
print(prof_eom)

# 11d) Intorno alle festività: da 3 giorni prima (k-3) a 3 dopo (k3), per giorno
#      della settimana in cui cade la festività. Serve a trovare i ponti.
#      Esclusi il periodo 20/12-10/01 (11e) e Pasqua (11g).
hol_dates <- data$Date[hol_any]
hol_tab <- data.frame(h = hol_dates) %>%
  dplyr::filter(!(month(h) == 12 & day(h) >= 20), !(month(h) == 1 & day(h) <= 10)) %>%
  mutate(holiday_weekday = factor(wday(h, week_start = 1), levels = 1:7, labels = wd_labels)) %>%
  tidyr::crossing(k = -3:3) %>%
  mutate(Date = h + k) %>%
  inner_join(diag_prof %>% select(Date, rel), by = "Date")
prof_hol <- hol_tab %>%
  group_by(holiday_weekday, k) %>% summarise(median = round(median(rel), 3), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = k, values_from = median, names_prefix = "k") %>%
  left_join(hol_tab %>% distinct(h, holiday_weekday) %>% count(holiday_weekday, name = "n_holidays"),
            by = "holiday_weekday")
print(prof_hol)

# 11e) Periodo natalizio: profilo per data dal 15/12 al 10/01
xmas_levels <- c(sprintf("12-%02d", 15:31), sprintf("01-%02d", 1:10))
prof_xmas <- diag_prof %>%
  mutate(date_mm_dd = format(Date, "%m-%d")) %>%
  dplyr::filter(date_mm_dd %in% xmas_levels) %>%
  mutate(date_mm_dd = factor(date_mm_dd, levels = xmas_levels)) %>%
  group_by(date_mm_dd) %>% summarise(median = round(median(rel), 3), n = n())
print(prof_xmas, n = 30)

# 11f) Agosto: profilo per giorno
prof_aug <- diag_prof %>% dplyr::filter(month(Date) == 8) %>%
  group_by(august_day = dom) %>% summarise(median = round(median(rel), 3))
print(prof_aug, n = 31)

# 11g) Finestra di Pasqua: da Venerdì Santo (k = -3) al mercoledì dopo Pasquetta (k = +2)
em_dates <- data$Date[em_vec == 1]
easter_days <- c("Good Friday", "Holy Saturday", "Easter Sunday", "Easter Monday",
                 "Easter Tuesday", "Easter Wednesday")
prof_easter <- data.frame(e = em_dates) %>%
  tidyr::crossing(k = -3:2) %>%
  mutate(Date = e + k) %>%
  inner_join(diag_prof %>% select(Date, rel), by = "Date") %>%
  group_by(k) %>%
  summarise(median = round(median(rel), 3), n = n()) %>%
  mutate(day = easter_days[k + 4]) %>%
  select(day, median, n)
print(prof_easter)

# 11h) Ponti e giorni speciali, per feriale / sabato / domenica
day_type_sp <- dplyr::case_when(
  holiday_sat_vec == 1                            ~ "Holiday on Saturday",
  good_friday_vec == 1                            ~ "Good Friday",
  holy_saturday_vec == 1                          ~ "Holy Saturday",
  easter_tuesday_vec == 1                         ~ "Easter Tuesday",
  bridge_mon_vec == 1                             ~ "Bridge Monday",
  bridge_fri_vec == 1                             ~ "Bridge Friday",
  md_all == "12-24"                               ~ "Christmas Eve",
  md_all == "12-31"                               ~ "New Year's Eve",
  md_all %in% c("08-14", "08-16")                 ~ "Aug 14 / Aug 16",
  md_all %in% xmas_workdays & !hol_any            ~ "Christmas period",
  TRUE ~ NA_character_)
prof_special <- diag_prof %>%
  mutate(special_day = day_type_sp[match(Date, data$Date)],
         day_class   = ifelse(wd <= 5, "Weekday", ifelse(wd == 6, "Saturday", "Sunday"))) %>%
  dplyr::filter(!is.na(special_day)) %>%
  group_by(special_day, day_class) %>%
  summarise(median = round(median(rel), 3), n = n(), .groups = "drop")
print(prof_special, n = 30)

# 11i) Grafico del profilo per giorno del mese
plot_prof_dom <- ggplot(diag_prof, aes(factor(dom), rel)) +
  geom_boxplot(fill = "grey80", color = "black", outlier.size = 0.5) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = "Day of month", y = "SA_CA / monthly median",
       title = "Residual intra-month pattern in SA_CA") +
  theme_clean
plot_prof_dom


################################################################################
#  PASSO 12 - ESPORTAZIONE E SALVATAGGIO                                       #
################################################################################
# Fogli: Daily, Monthly e, con coefficienti variabili, la tabella dei
# coefficienti di calendario a fine anno (pronta per le slide).

export_list <- list(Daily = daily, Monthly = monthly)
if (!joint_calendar) {
  export_list$Calendar_coef_by_year <- data.frame(Regressor = rownames(coef_by_year),
                                                  coef_by_year, check.names = FALSE)
}

write.xlsx(export_list, file = "seasonal_adjustment_outputs_Electricity_IT.xlsx", overwrite = TRUE)

save.image(file = "workspace_electricity_demand_IT.RData")








################################## FIGURE ######################################

################################################################################
#                                                                              #
#   DOMANDA ELETTRICA - FIGURE DEGLI EFFETTI DI CALENDARIO VARIABILI           #
#                                                                              #
################################################################################
# Da incollare in CODA allo script finale dell'elettricità (o subito dopo il
# PASSO 5): usa i coefficienti già stimati nel PASSO 5 (ss, cal_reg), quindi
# nessuna nuova stima e figure coerenti con il modello usato per la SA.
# Richiede joint_calendar = FALSE e il file calendar_tools.R nella cartella
# di lavoro (per plot_calendar_figure_a4).
#
# Figure prodotte (PNG 300 dpi + PDF vettoriale) nella cartella fig_dir:
#   electricity_tv_all        tre pannelli uno sotto l'altro (pagina A4)
#   electricity_tv_holidays   festività nazionali
#   electricity_tv_bridges    ponti e giorni di semi-chiusura
#   electricity_tv_other      sabato festivo, festività infrasettimanali,
#                             finestra di Pasqua
# Stessa grafica delle figure del turismo e delle nascite.
################################################################################

source("calendar_tools.R")

stopifnot(!joint_calendar, exists("ss"), exists("cal_reg"))

fig_dir <- "figures_electricity"
dir.create(fig_dir, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1) Coefficienti variabili dal PASSO 5, nel formato di plot_calendar_figure_a4
#    (i regressori sono le ultime ncol(cal_reg) colonne degli stati)
# ------------------------------------------------------------------------------
reg_idx <- (ncol(ss) - ncol(cal_reg) + 1):ncol(ss)
stopifnot(identical(as.integer(reg_idx), as.integer(cal_idx)))   # stesso indice del PASSO 5

tv_el <- list(beta = ss[, reg_idx, drop = FALSE])
colnames(tv_el$beta) <- colnames(cal_reg)

# ------------------------------------------------------------------------------
# 2) Gruppi di regressori (un pannello per gruppo)
# ------------------------------------------------------------------------------
grp_el_holidays <- c("New Year's Day", "Epiphany", "Easter Sunday", "Easter Monday",
                     "Liberation Day", "Labour Day", "Republic Day", "Assumption of Mary",
                     "All Saints' Day", "Immaculate Conception", "Christmas", "St. Stephen's Day")
grp_el_bridges  <- c("Bridge_Mon", "Bridge_Fri", "Christmas_Eve_Wkd", "New_Years_Eve_Wkd",
                     "Assumption_Adjacent_Wkd", "Christmas_Period_Wkd")
grp_el_other    <- c("Holiday_Sat", "Holiday_Midweek", "Good_Friday", "Holy_Saturday", "Easter_Tuesday")

# Controllo: ogni regressore del modello è in uno e un solo gruppo
stopifnot(setequal(c(grp_el_holidays, grp_el_bridges, grp_el_other), colnames(cal_reg)),
          !anyDuplicated(c(grp_el_holidays, grp_el_bridges, grp_el_other)))

# ------------------------------------------------------------------------------
# 3) Figura unica con i tre pannelli (pagina A4 del paper)
# ------------------------------------------------------------------------------
plot_calendar_figure_a4(
  data$Date, tv_el, groups = list(grp_el_holidays, grp_el_bridges, grp_el_other), log = log,
  units = "thousand MWh", divide_by = 1000,
  main = "Electricity demand: time-varying calendar effects",
  leg_cols = c(3, 2, 3), width_cm = 16, height_cm = 22,
  file_stub = file.path(fig_dir, "electricity_tv_all"))

# ------------------------------------------------------------------------------
# 4) Figure singole, una per gruppo, ciascuna con il suo titolo
# ------------------------------------------------------------------------------
single_figs <- list(
  list(vars = grp_el_holidays, cols = 3, stub = "electricity_tv_holidays",
       main = "Electricity demand: national holidays"),
  list(vars = grp_el_bridges,  cols = 2, stub = "electricity_tv_bridges",
       main = "Electricity demand: bridges and semi-closure days"),
  list(vars = grp_el_other,    cols = 3, stub = "electricity_tv_other",
       main = "Electricity demand: Saturday and midweek holidays, Easter window"))

for (f in single_figs) {
  plot_calendar_figure_a4(
    data$Date, tv_el, groups = list(f$vars), log = log,
    units = "thousand MWh", divide_by = 1000,
    main = f$main, leg_cols = f$cols, width_cm = 16, height_cm = 10,
    file_stub = file.path(fig_dir, f$stub))
}













