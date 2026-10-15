################################################################################
#                                                                              #
#   DESTAGIONALIZZAZIONE GIORNALIERA DELLA FATTURAZIONE ELETTRONICA            #
#   Note metodologiche e storia delle modifiche                                #
#                                                                              #
################################################################################
#
# OBIETTIVO
# ---------
# Destagionalizzare la serie giornaliera della fatturazione elettronica
# (imponibile, €), proxy del fatturato ufficiale mensile, per dimostrare
# l'utilità del dato giornaliero destagionalizzato. Il valore aggiunto atteso
# è la tempestività: una serie giornaliera destagionalizzata e corretta per
# il calendario (SA_CA) rende confrontabili anche i mesi parziali, e permette
# quindi di stimare l'andamento del mese prima che si chiuda (nowcasting).
# Condizione necessaria: la SA_CA deve essere "piatta" all'interno del mese.
#
# PROCEDURA (versione finale)
# ---------------------------
# 1. Test log/livelli (AICc): la serie è trattata in logaritmi.
# 2. Pre-trattamento (fractionalAirlineEstimation, periodi 7 e 365.2425) con
#    outlier AO/WO/LS e regressori di calendario STIMATI CONGIUNTAMENTE al
#    modello stagionale (interruttore joint_calendar = TRUE).
# 3. Decomposizione AMB sequenziale sulla serie linearizzata:
#    prima periodo 7 (giorno della settimana), poi 365.2425 (giorno dell'anno).
#    Gli eventuali LS vengono reinseriti nella SA.
# 4. Output: SA_CA (destagionalizzata e corretta per il calendario) e
#    SA (= SA_CA con il fattore di calendario riapplicato), giornaliere
#    e aggregate a livello mensile (media), esportate in Excel.
#
# SPECIFICAZIONE DEI REGRESSORI DI CALENDARIO (32 in totale)
# ----------------------------------------------------------
# - 12 festività nazionali italiane. Le festività cadute di sabato o domenica
#   sono azzerate (trattate come un weekend normale); eccezione Pasqua, sempre
#   di domenica, mantenuta con il proprio effetto e ricavata dal Lunedì
#   dell'Angelo.                                                -> 12 regressori
# - Giorni feriali (_Wkd): EoM (ultimo giorno), EoM-1, ..., EoM-5,
#   Day1, Day15, Day20                                          -> 9 regressori
# - Sabato (_Sat): EoM, EoM-1, Day1, Day15, Day20                -> 5 regressori
# - Domenica (_Sun): EoM, Day1, Day15, Day20                     -> 4 regressori
# - Sun_before_EoM: domenica cadente da 2 a 5 giorni prima della
#   fine del mese (un unico regressore)                          -> 1 regressore
# - LastWorkDay_Fri: venerdì che precede un fine mese di sabato (EoM-1) o di
#   domenica (EoM-2), cioè l'ultimo giorno lavorativo del mese   -> 1 regressore
# I coefficienti sono fissi nel tempo. Tutti significativi (|t| minimo = 4.5).
#
# COSA ABBIAMO SCOPERTO E CORRETTO (in ordine cronologico)
# --------------------------------------------------------
# a) Effetto fine mese diverso secondo il giorno della settimana.
#    L'ultimo giorno del mese vale molte volte un giorno normale, ma il
#    moltiplicatore è molto più alto se cade di domenica: le imprese datano le
#    fatture all'ultimo giorno del mese anche se è domenica, e una domenica
#    normale ha pochissime fatture. Un unico coefficiente era un compromesso
#    sbagliato per tutti i giorni.
#
# b) Stagionalità intra-mensile residua nella SA_CA.
#    Il profilo della SA_CA per giorno del mese (valore / mediana del mese)
#    mostrava picchi al giorno 1 (+17%) e al giorno 15 (+27%), una "rampa"
#    nella settimana prima del fine mese (fino a +13%) e una sovra-correzione
#    degli ultimi giorni (fino a -18%).
#    Il picco del 15 ha una spiegazione normativa: la fattura differita va
#    emessa entro il 15 del mese successivo alla consegna. Il picco del 20
#    (insieme al 10 e al fine mese) potrebbe riflettere una fatturazione a
#    cadenza decadale: è un'ipotesi, non verificata.
#
# c) Stima in due passi non coerente -> stima congiunta.
#    In origine il calendario era stimato con un modello separato (rjd3sts,
#    airline con sola stagionalità settimanale, coefficienti variabili nel
#    tempo) e la serie corretta passava poi al pre-trattamento. Due problemi:
#    - la varianza stimata dei coefficienti è risultata nulla, quindi i
#      coefficienti erano in realtà costanti e il vantaggio del modello
#      "time-varying" non c'era;
#    - i coefficienti erano stimati rispetto a un modello diverso da quello
#      della decomposizione, e un livello locale molto flessibile assorbiva
#      gli effetti graduali (la rampa), che i dummy non riuscivano a catturare.
#    Soluzione: regressori passati come x a fractionalAirlineEstimation
#    (joint_calendar = TRUE). Il vecchio approccio resta nello script ed è
#    riattivabile con joint_calendar = FALSE.
#
# d) Sovra-correzione sistematica dei giorni con un dummy unico.
#    Ogni giorno corretto con un solo dummy risultava sovra-corretto del
#    10-15% nei feriali e sotto-corretto nel weekend (es. Day15: feriale 0.89,
#    domenica 2.12; EoM-1: sabato 2.67, domenica 0.51). Causa: nel weekend il
#    confronto è con un giorno a bassissima fatturazione, quindi i rapporti
#    sono molto più alti e spingono verso l'alto il coefficiente comune.
#    Inoltre il weekend prima di un fine mese feriale è un weekend normale,
#    mentre il weekend che È la fine del mese non lo è.
#    Soluzione: sdoppiamento dei regressori in feriale/sabato/domenica.
#
# e) Ultimo giorno lavorativo.
#    Quando il mese finisce nel weekend, il venerdì precedente concentra la
#    fatturazione: quei venerdì risultavano sotto-corretti (1.63-1.78) e gli
#    altri EoM-1/EoM-2 feriali sovra-corretti (0.86-0.88).
#    Soluzione: regressore LastWorkDay_Fri. Dopo: tutti tra 0.99 e 1.04.
#
# f) Estensione: EoM fino a EoM-5 e Day20, per eliminare la rampa residua a
#    4-5 giorni dalla fine del mese (+8%, +6%) e il picco del 20 (+8%).
#
# g) Semplificazione (da 40 a 32 regressori).
#    - Sabati da EoM-2 a EoM-5 non significativi (t tra -0.7 e 0.3):
#      eliminati. La rampa pre-fine mese riguarda solo i giorni lavorativi;
#      un sabato qualche giorno prima della fine del mese è un sabato normale.
#    - Domenica EoM-1 al limite della significatività (t = 1.78), effetto
#      piccolo (+11%): eliminata.
#    - Domeniche da EoM-2 a EoM-5 significative ma con coefficienti simili
#      (tra -0.20 e -0.28): accorpate in un unico regressore Sun_before_EoM.
#      Effetto documentato empiricamente, senza una spiegazione consolidata.
#
# h) Festività cadute nel weekend.
#    Nella costruzione originale (rjd3toolkit::holidays, type = "All") le
#    festività restavano attive anche di sabato e domenica: il modello applicava
#    a un Natale di domenica l'effetto di un Natale feriale (x0.03) e il
#    pre-trattamento compensava con un outlier. 11 dei 13 outlier erano
#    festività nel weekend, con coefficienti che annullavano quasi esattamente
#    quello della festività.
#    Soluzione: festività azzerate a mano quando cadono nel weekend; Pasqua
#    (sempre di domenica) ricavata dal Lunedì dell'Angelo. L'opzione
#    type = "Skip" è stata scartata perché azzerava anche Pasqua (regressore
#    nullo, stima impossibile). Controlli automatici sulla costruzione.
#    Effetti: outlier da 13 a 8; risolto il residuo del giorno 1 nel weekend
#    (da ~0.85 a ~1: Capodanno, 1 maggio e Ognissanti nel weekend
#    contaminavano la stima di Day1_Sat/Sun); sparito un cambio di livello
#    spurio (LS 24/12/2019), sostituito da cambi di livello con un chiaro
#    significato economico (vedi sezione OUTLIER).
#
# i) Correzioni tecniche minori:
#    - rjd3filters caricato per primo: la sua funzione filter() copriva
#      dplyr::filter. Usare comunque sempre dplyr::filter in modo esplicito.
#    - rjd3toolkit copre stats::aggregate: usare stats::aggregate esplicito.
#    - grafico degli effetti di calendario reso dinamico (numero di regressori
#      qualsiasi); colori della legenda DOW/DOY corretti.
#
# LETTURA DEI COEFFICIENTI (fattori moltiplicativi = exp(coef))
# -------------------------------------------------------------
#                                   Feriale   Sabato   Domenica
#   Ultimo giorno del mese           x 8.6    x 30      x 69
#   Giorno 1                         x 1.4    x 1.9     x 3.9
#   Giorno 15 (fattura differita)    x 1.37   x 2.1     x 3.6
#   Giorno 20                        x 1.16   x 1.25    x 1.29
#
#   Rampa pre-fine mese (feriali): EoM-1 x1.44, EoM-2 x1.33, EoM-3 x1.33,
#                                  EoM-4 x1.21, EoM-5 x1.13
#   Sabato penultimo giorno (mese che finisce di domenica): x6.1
#   Ultimo giorno lavorativo (venerdì, mese che finisce nel weekend): x2.1
#   Domenica nei 2-5 giorni prima della fine del mese: x0.82 (-18%)
#   Festività (feriali): da x0.04 (Natale, -96%) a x0.31 (Ferragosto, -69%:
#   agosto è già un mese di bassa attività). Pasqua: x0.67 rispetto a una
#   domenica normale.
#
# ATTENZIONE nella lettura: i moltiplicatori del weekend sono rapporti rispetto
# a un sabato o una domenica NORMALE, in cui si fattura pochissimo. Non
# significano che di domenica si fatturi più che in un giorno feriale.
#
# OUTLIER (8)
# -----------
# - LS 09/03/2020 (-34%): lockdown nazionale per il Covid-19.
# - LS 02/06/2020 (+32%): riaperture dopo il lockdown.
# - WO 06/12/2020 e AO 05/04/2020: periodo Covid.
# - AO 26/12/2020 (-67%): Santo Stefano di sabato, più basso di un sabato
#   normale (periodo natalizio).
# - AO 03/02/2019 e 02/02/2020 (+228%, +218%): prime domeniche di febbraio,
#   simili tra loro; causa da verificare.
# - AO 08/02/2025 (coefficiente -19.3 in log): valore sostituito con 1
#   perché <= 0 all'origine. Probabile dato mancante: da verificare alla fonte.
#
# DIAGNOSTICA FINALE (sezione in fondo allo script)
# -------------------------------------------------
# Indicatore: SA_CA del giorno / mediana della SA_CA del mese (atteso ~1).
# - Profilo per giorni alla fine del mese (to_eom 0-9): tra 0.996 e 1.04.
# - Giorni con regressore dedicato, per feriale/sabato/domenica: 26 celle su
#   27 tra 0.95 e 1.05 (la restante, EoM-5 domenica, a 0.946).
# - Profilo per giorno del mese: entro +/-5%, con due soli giorni al limite
#   (giorno 9: 0.94; giorno 2: 0.95), lasciati come residui minori.
# - Verifica dell'estrazione dell'effetto di calendario: 99.4% dei giorni
#   coerenti (serie osservata - linearizzata - calendario = soli outlier).
#
################################################################################
#                                                                              #
#   COSTRUIRE UN BUON CALENDARIO PER DATI GIORNALIERI                          #
#   Procedimento operativo e good practice                                     #
#                                                                              #
################################################################################
#
# FASE 1 - ESPLORARE IL DATO GREZZO
#   - Boxplot per giorno della settimana, giorno del mese e mese.
#   - Periodogramma con le armoniche di 7, 30.44 e 365.24 giorni: picchi alle
#     frequenze mensili segnalano effetti legati al giorno del mese.
#   - Individuare i giorni anomali ricorrenti e chiedersi PERCHÉ lo sono.
#
# FASE 2 - PARTIRE DA UN CALENDARIO MINIMO E MOTIVATO
#   - Festività nazionali, attive solo nei giorni lavorativi (una festività
#     di domenica non è una domenica "speciale", salvo eccezioni come Pasqua).
#   - Effetti noti del dominio: fine mese, scadenze fiscali o normative
#     (es. fattura differita entro il 15), cicli amministrativi.
#   - Ogni regressore deve avere una giustificazione economica o normativa,
#     oppure un effetto sistematico su tutto il campione.
#
# FASE 3 - STIMARE IL CALENDARIO INSIEME AL MODELLO STAGIONALE
#   - Regressori nel pre-trattamento, non in un modello separato: stime in due
#     passi con modelli diversi producono coefficienti incoerenti con la
#     decomposizione.
#   - Usare coefficienti variabili nel tempo solo se i dati lo confermano
#     (varianza stimata diversa da zero); altrimenti coefficienti fissi.
#
# FASE 4 - DIAGNOSTICA SULLA SERIE CORRETTA (SA_CA), NON SUI COEFFICIENTI
#   - Indicatore: SA_CA / mediana della SA_CA del mese (atteso ~1).
#   - Due profili complementari:
#       * per giorno del mese (1..31) -> date fisse (1, 15, 20...);
#       * per giorni mancanti alla fine del mese (0, 1, 2, ...) -> effetti di
#         fine mese. Il profilo per giorno del mese da solo mescola mesi di
#         lunghezza diversa (il 29 è l'ultimo giorno a febbraio bisestile,
#         il terzultimo nei mesi di 31).
#   - Usare le mediane, robuste a outlier e mesi eccezionali.
#
# FASE 5 - DISAGGREGARE I RESIDUI PER GIORNO DELLA SETTIMANA
#   - Per ogni giorno con regressore, separare feriale / sabato / domenica.
#   - Segnale tipico di un regressore mal specificato: sovra-correzione nei
#     feriali (< 1) e sotto-correzione nel weekend (> 1). Un coefficiente
#     unico è un compromesso tra giorni con livelli di base molto diversi.
#   - Rimedio: sdoppiare il regressore per tipo di giorno.
#
# FASE 6 - LEGGERE I RESIDUI CON LOGICA ECONOMICA
#   - Il calendario "tecnico" (quanti giorni mancano alla fine) non basta:
#     conta il calendario "operativo" delle imprese.
#   - Esempi emersi: il weekend che È la fine del mese è attivo, il weekend
#     che la precede no; quando il mese finisce nel weekend, la fatturazione
#     si sposta sull'ultimo giorno lavorativo (venerdì).
#   - Formulare un'ipotesi, verificarla con una tabella mirata (es. venerdì vs
#     altri feriali), solo dopo aggiungere il regressore.
#
# FASE 7 - UNA MODIFICA ALLA VOLTA
#   - Aggiungere un regressore (o un gruppo omogeneo) per volta, rilanciare,
#     rifare TUTTI i profili: una correzione può spostare il problema sui
#     giorni vicini (es. un picco che diventa un buco, il giorno dopo che sale).
#   - Controllare che ogni regressore sia attivo in un numero sufficiente di
#     giorni (indicativamente almeno 10-15 casi per cella).
#
# FASE 8 - CONTROLLI DI COERENZA DEL MODELLO
#   - Costruzione dei regressori: verificare che ogni regressore cada nei
#     giorni attesi (es. Pasqua sempre di domenica), che nessuno sia sempre
#     a zero e che la matrice abbia rango pieno. Non dare per scontato il
#     comportamento delle opzioni dei pacchetti: verificarlo sui dati.
#   - Lista degli outlier: outlier ricorrenti su date di calendario (festività,
#     fine mese) segnalano un calendario mal specificato. In un modello ben
#     specificato gli outlier raccontano eventi (qui: lockdown e riaperture
#     del 2020) o problemi nei dati.
#   - Estrazione dell'effetto di calendario: serie osservata - linearizzata -
#     calendario deve contenere solo gli outlier (differenze giornaliere nulle
#     quasi ovunque). Garantisce che la serie SA sia corretta.
#
# FASE 9 - SEMPLIFICARE
#   - Rivedere la significatività dei coefficienti una volta che la
#     diagnostica è soddisfacente.
#   - Eliminare i regressori non significativi: se l'effetto stimato è nullo,
#     toglierli non cambia la serie corretta ma rende il modello più
#     parsimonioso e le altre stime più precise.
#   - Accorpare regressori con coefficienti simili in un unico regressore.
#   - Leggere anche i "non significativi": dicono dove l'effetto NON c'è
#     (qui: nessuna rampa pre-fine mese nei sabati).
#   - Dopo la semplificazione, ricontrollare i profili.
#
# FASE 10 - SAPERSI FERMARE
#   - Criterio pratico: profili entro +/-5%.
#   - Non inseguire residui basati su pochi casi (es. 20-25 osservazioni in
#     7 anni): il rischio di sovra-adattamento supera il beneficio. Spesso,
#     inoltre, si risolvono correggendo un errore strutturale altrove
#     (qui: il giorno 1 nel weekend, sistemato dalla correzione delle festività).
#   - Non aggiungere un dummy per ogni giorno che si discosta da 1:
#     ogni regressore deve reggere la domanda "perché questo giorno?".
#   - Documentare i residui non corretti come limiti noti.
#
# BUONE REGOLE TECNICHE
#   - Picchi netti di un singolo giorno -> regressori (dummy).
#     Forme graduali su più giorni -> regressori su più giorni stimati
#     congiuntamente, oppure una componente stagionale dedicata.
#   - Giorno della settimana con lubridate::wday(..., week_start = 1):
#     non dipende dalla lingua del sistema.
#   - Rendere grafici e controlli indipendenti dal numero di regressori.
#   - Inserire controlli automatici (stopifnot) sulla costruzione del
#     calendario: un errore silenzioso nei regressori si propaga a tutto.
#   - Presentare i coefficienti come fattori moltiplicativi, chiarendo sempre
#     rispetto a quale giorno "normale" sono calcolati.
#
################################################################################

# >>> MODIFICA: rjd3filters caricato PER PRIMO. Ha una sua funzione filter() che,
#     caricata dopo dplyr, copriva dplyr::filter (causa dell'errore "oggetto non trovato").
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

# Input definitions 

# base_path                 <- "C:\\Users\\alessandro.piovani\\Desktop\\NTTS_high_freq_daily"
base_path                 <- "C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\NTTS_high_freq_daily"
input_data_path           <- paste0(base_path,"\\Fatturazione_elettronica_e_fatturato_industria\\output_serie_storica_SUM.csv")
monthly_SA_benchmark_path <- paste0(base_path,"\\Fatturazione_elettronica_e_fatturato_industria\\sa_new_fatturato_industria.csv")
setwd(base_path)

blank_par <- par(no.readonly = TRUE) # for plots


# load RData
# load("workspace_fatturazione_SUM.RData")

# source
source("utilities.R")
source("revision.R")
source("stability.R")

label          <- "Electronic invoicing (taxable amount, €)"
label_original <- "Electronic invoicing (taxable amount, €)"
lab <- label
date_start <- "2019-01-01"
date_end   <- "2025-12-31"

col_to_analyze <- "imposta" # Cambiarlo per analizzare certi settori specifici es. "sett00num_fatture"

# ------------------------------------------------------------------------------
# >>> NUOVO: INTERRUTTORI
# ------------------------------------------------------------------------------
# joint_calendar = TRUE  -> effetti di calendario stimati DENTRO il pre-trattamento
#                           (fractionalAirlineEstimation), insieme al modello stagionale.
#                           Coefficienti fissi. Scelta consigliata.
# joint_calendar = FALSE -> vecchio approccio in due passi (rjd3sts con coefficienti
#                           variabili nel tempo, poi pre-trattamento).
joint_calendar <- TRUE

# use_dom_step = FALSE -> nessuna componente intra-mensile (passo 1)
# use_dom_step = TRUE  -> aggiunge la componente intra-mensile, periodo 30.436875 (passo 2)
use_dom_step <- FALSE


# import data
data_input      <- read.csv2(input_data_path)

data <- data.frame(Date  = data_input$data,
                   Value = as.numeric(data_input[[col_to_analyze]]))
data$Date <- as.Date(data$Date)


# restrict time period
data = data %>% 
  dplyr::filter(Date >= as.Date(date_start))%>% 
  dplyr::filter(Date <= as.Date(date_end)) 


print("Values <= 0, substituted with 1")
print(sum(data$Value <= 0))
data$Value[data$Value <= 0] <- 1

################################################################################
######################## Seasonality inspection ################################
################################################################################
######################## raw series
original_data <- ggplot(data, aes(x = Date, y = Value)) +
  geom_line() +
  labs(x = "Year", y = label_original, title = "Unadjusted data")

original_data

######################## boxplots
Sys.setlocale("LC_TIME", "C")

data <- data %>%
  mutate(
    day = factor(
      weekdays(Date), 
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
      ordered = TRUE
    ),
    month = factor(
      month.name[month(Date)],  
      levels = month.name,      
      ordered = TRUE
    ),
    day_of_month = factor(
      day(Date),
      levels = 1:31,
      ordered = TRUE
    ),
    year = factor(year(Date), ordered = TRUE)  
  )

box_plot_dow <- ggplot(data, aes(x = day, y = Value)) +
  geom_boxplot(fill = "grey80", color = "black") +
  labs(
    x = "Day",
    y = label_original,
    title = "Day-of-the-week pattern"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    plot.title = element_text(hjust = 0.5)  # Centra il titolo
  )

box_plot_dow

box_plot_doy <- ggplot(data, aes(x = month, y = Value)) +
  geom_boxplot(fill = "grey80", color = "black") +
  labs(
    x = "Month",
    y = label_original,
    title = "Day-of-the-year pattern"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    plot.title = element_text(hjust = 0.5)  # Centra il titolo
  )

box_plot_doy


box_plot_dom <- ggplot(data, aes(x = day_of_month, y = Value)) +
  geom_boxplot(fill = "grey80", color = "black") +
  labs(
    x = "Day of month",
    y = label_original,
    title = "Day-of-the-month pattern"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

box_plot_dom


data=data %>% 
  select(-day,-month,-year, -day_of_month)


######################## seasonality Tests

# res1 <- run_tests(data$Value, data_name = "original", test_name="CanovaHansen")
# res2 <- run_tests(data$Value, data_name = "original", test_name="F")
# res3 <- run_tests(data$Value, data_name = "original", test_name="KruskalWallis")

# Merge the results


# all_seas_tests_original_data <- list()
# all_seas_tests_original_data[["CanovaHansen"]]  <- round(res1[,c("value", "pvalue")],2)
# all_seas_tests_original_data[["F"]]             <- round(res2[,c("value", "pvalue")],2)
# all_seas_tests_original_data[["KruskalWallis"]] <- round(res3[,c("value", "pvalue")],2)

# all_seas_tests_original_data







# ==============================================================================
# SPECTRAL ANALYSIS: PERIODOGRAMS WITH CORE PERIODICITIES & EXPANDED HARMONICS
# ==============================================================================

# 1. Compute the raw periodogram
spec_non_seasonal <- spec.pgram(data$Value, log = FALSE, plot = FALSE)

# 2. Define theoretical frequencies with expanded harmonics
freq_weekly  <- (1:4) / 7               # First 4 harmonics for the 7-day cycle
freq_monthly <- (1:15) / 30.436875      # First 15 harmonics for the 30.4-day cycle
freq_annual  <- (1:3) / 365.2425        # First 3 harmonics for the 365.2-day cycle


# ------------------------------------------------------------------------------
# APPROACH 1: BASE R PLOT (WITH ENGLISH LEGEND)
# ------------------------------------------------------------------------------

# Render the main spectral density line
plot(spec_non_seasonal$freq, spec_non_seasonal$spec, type = "l", 
     col = "blue", lwd = 2,
     xlab = "Frequency", ylab = "Spectral density")

# Superimpose the harmonic reference lines
abline(v = freq_annual, col = "gray60", lty = 4)    # Annual
abline(v = freq_monthly, col = "darkred", lty = 3)  # Monthly
abline(v = freq_weekly, col = "gray20", lty = 2)   # Weekly

# Graph Title
title(main = "Raw Series - Periodogram", font.main = 1)

# English Legend
legend("topright", 
       legend = c("Weekly (7d harmonics: 1-4)", "Monthly (30.4d harmonics: 1-15)", "Annual (365.2d harmonics: 1-3)"), 
       col = c("gray20", "darkred", "gray60"), 
       lty = c(2, 3, 4), 
       lwd = 1.5,
       bg = "white",
       bty = "o")

########################### KDE

ts_raw_data <- with(data[order(data$Date), ],
                    ts(Value,
                       start = c(as.numeric(format(min(Date), "%Y")),
                                 as.numeric(format(min(Date), "%j"))),
                       frequency = 365))

xlim_range <- c(-0.5, 0.5) # adjust them basing on the plot ranges of these data
ylim_range <- c(0, 7.5)

KDE_raw   <- plot_kde_acf(raw_data = ts_raw_data, lags = c(7, 30, 365), window_cycles =  c(4,3,2), xlim_min = xlim_range[1], xlim_max = xlim_range[2], ylim_min = ylim_range[1], ylim_max = ylim_range[2], show_legend = TRUE, title_label = "KDE of rolling ACF for raw data")
KDE_raw


################################################################################
##############################  Log-level test #################################
################################################################################

# check all values are larger than zero
data <- data %>%
  dplyr::mutate(Value = dplyr::if_else(Value <= 0, 1, Value))

# compare aicc of additive and multiplicative models
# jspec     <-  rjd3highfreq::.extended_airline_spec(periodicities = c(7),differencing = -1, ar = FALSE, toint = FALSE) #differencing=-1 -> auto differencing
jspec     <-  rjd3highfreq::.extended_airline_spec(periodicities = c(7,365.2425),differencing = -1, ar = FALSE, toint = FALSE) #differencing=-1 -> auto differencing
jregarima <-  rjd3highfreq::.extended_airline_regarima(y = data$Value, jspec=jspec)
aiccs     <-  rjd3highfreq::.extended_airline_loglevel(jregarima = jregarima, jspec = jspec) # {ll.getAICcLevel(), ll.getAICcLog()}, in Java code

if(aiccs[1] <= aiccs[2]) {
  print("No log transformation applied")
  log <- FALSE
}  else {
  print("Log transformation applied")
  log <- TRUE
}



aiccs[1]
aiccs[2]

# log <- FALSE # Per forzare a non usare il log

################################################################################
########################## Pre-processing ######################################
################################################################################
########################## calendar effects
# Italian calendar
italian_calendar <- rjd3toolkit::national_calendar(days = list(
  rjd3toolkit::fixed_day(4, 25),            # Liberation Day - April 25th
  rjd3toolkit::fixed_day(6, 2),             # Republic Day - June 2nd
  rjd3toolkit::fixed_day(12, 8),            # Immaculate Conception - December 8th
  rjd3toolkit::fixed_day(12, 26),           # St. Stephen's Day - December 26th
  rjd3toolkit::fixed_day(1, 6),             # Epiphany
  rjd3toolkit::special_day('NEWYEAR'),      # New Year's Day - January 1st
  rjd3toolkit::special_day('EASTERMONDAY'), # Easter Monday (varies) - Day after Easter Sunday
  rjd3toolkit::special_day('MAYDAY'),       # Labour Day - May 1st
  rjd3toolkit::special_day('ASSUMPTION'),   # Assumption of Mary - August 15th
  rjd3toolkit::special_day('ALLSAINTSDAY'), # All Saints' Day - November 1st
  rjd3toolkit::special_day('CHRISTMAS'), # Christmas Day - December 25th
  rjd3toolkit::special_day('EASTER')
))
# >>> MODIFICA (v7): festività nel weekend azzerate A MANO.
#     Con type = "All" le festività cadute di sabato/domenica restavano attive: il
#     modello applicava a un Natale di domenica l'effetto di un Natale feriale (x0.03)
#     e il pre-trattamento compensava con un outlier AO (11 dei 13 outlier della v5
#     erano festività nel weekend). La v6 usava type = "Skip", ma produceva una
#     colonna di Pasqua tutta a zero (errore "Gls failed").
#     Soluzione trasparente e indipendente dalle opzioni del pacchetto:
#     - si parte dalla versione "All" (quella già usata e funzionante);
#     - le 11 festività diverse da Pasqua vengono azzerate quando cadono nel weekend;
#     - Pasqua (sempre di domenica, effetto reale di circa -33% rispetto a una
#       domenica normale) viene ricavata dal Lunedì dell'Angelo: è il giorno prima.
cal_all <- rjd3toolkit::holidays(
  calendar = italian_calendar,   # Use the Italian calendar
  start = min(data$Date), length = nrow(data),
  type = "All", nonworking = c(6,7))

wd_h <- lubridate::wday(data$Date, week_start = 1)   # 1 = lunedì ... 7 = domenica

cal_reg <- cal_all
cal_reg[, 1:11] <- cal_all[, 1:11] * (wd_h <= 5)     # festività solo nei giorni feriali

easter_monday <- as.numeric(cal_all[, 7])            # 7a colonna = Easter Monday
cal_reg[, 12]  <- c(easter_monday[-1], 0)            # 12a colonna = Easter (giorno prima)

# Controlli automatici sulla costruzione delle festività
stopifnot(
  sum(easter_monday) > 0,                                   # Lunedì dell'Angelo presente
  all(wd_h[easter_monday == 1] == 1),                       # ... sempre di lunedì
  all(wd_h[cal_reg[, 12] == 1] == 7),                       # Pasqua sempre di domenica
  sum(cal_reg[, 12]) == sum(easter_monday),                 # stesso numero di occorrenze
  all(wd_h[rowSums(cal_reg[, 1:11, drop = FALSE]) > 0] <= 5) # nessuna festività nel weekend
)

colnames(cal_reg) <- c("Liberation Day", "Republic Day", "Immaculate Conception", "St. Stephen's Day", "Epiphany",
                       "New Year’s Day", "Easter Monday", "Labour Day", "Assumption of Mary", "All Saints' Day", "Christmas", "Easter")


####### End of Month Effect + Day 1 + Day 15

# >>> MODIFICA (v4): regressori di fine mese estesi a EoM-5 (EoM, EoM-1, ..., EoM-5)
#     per catturare la "rampa" residua a 4-5 giorni dalla fine del mese
eom_regressors <- generate_eom_regressors(data$Date, num_days_before = 5) 

# >>> NUOVO: regressori base = fine mese + giorno 1 + giorno 15 + giorno 20 (v4)
#     (giorno 15: fattura differita da emettere entro il 15 del mese successivo;
#      giorno 20: picco sistematico residuo, possibile fatturazione a cadenza decadale)
base_reg <- as.matrix(cbind(eom_regressors,
                            Day1  = as.numeric(day(data$Date) == 1),
                            Day15 = as.numeric(day(data$Date) == 15),
                            Day20 = as.numeric(day(data$Date) == 20)))

# >>> NUOVO: ogni regressore base viene sdoppiato in tre versioni:
#     giorno feriale (Wkd), sabato (Sat), domenica (Sun).
#     Motivo: l'effetto rispetto a un giorno "normale" è molto diverso a seconda
#     del giorno della settimana (es. fine mese di domenica ~53 volte una domenica
#     normale, fine mese feriale ~10 volte un feriale normale).
#     wday() con week_start = 1 non dipende dalla lingua del sistema (6 = sabato, 7 = domenica).
wd       <- lubridate::wday(data$Date, week_start = 1)
day_type <- ifelse(wd == 7, "Sun", ifelse(wd == 6, "Sat", "Wkd"))

split_reg <- do.call(cbind, lapply(c("Wkd", "Sat", "Sun"), function(g) {
  m <- base_reg * (day_type == g)
  colnames(m) <- paste0(colnames(base_reg), "_", g)
  m
}))

# >>> MODIFICA (v5): semplificazione dei regressori sulla base della significatività
#     - eliminati i sabati da EoM-2 a EoM-5 (t tra -0.7 e 0.3: nessun effetto;
#       la rampa pre-fine mese riguarda solo i giorni lavorativi)
#     - eliminata la domenica EoM-1 (t = 1.78, effetto piccolo e non sistematico)
#     - accorpate le domeniche da EoM-2 a EoM-5 in un unico regressore
#       (coefficienti simili, tra -0.20 e -0.28: un parametro invece di quattro)
drop_cols <- c(paste0("eom_minus_", 2:5, "_Sat"), "eom_minus_1_Sun")
sun_cols  <- paste0("eom_minus_", 2:5, "_Sun")
stopifnot(all(c(drop_cols, sun_cols) %in% colnames(split_reg)))  # controllo nomi

Sun_before_EoM <- rowSums(split_reg[, sun_cols])
split_reg <- split_reg[, !(colnames(split_reg) %in% c(drop_cols, sun_cols))]
split_reg <- cbind(split_reg, Sun_before_EoM = Sun_before_EoM)

# >>> NUOVO (v3): ultimo giorno lavorativo quando il mese finisce nel weekend.
#     È il venerdì che precede un fine mese di sabato (EoM-1) o di domenica (EoM-2):
#     le imprese vi concentrano la fatturazione. Senza questo regressore, quei venerdì
#     risultavano sotto-corretti (~1.6-1.8) e gli altri EoM-1/EoM-2 feriali sovra-corretti (~0.87).
#     L'effetto si somma a quello di EoM-1_Wkd / EoM-2_Wkd.
to_eom_days <- as.integer(days_in_month(data$Date)) - day(data$Date)
LastWorkDay_Fri <- as.numeric(wd == 5 & to_eom_days %in% 1:2)

# >>> MODIFICA: matrice finale dei regressori di calendario
cal_reg <- as.matrix(cbind(cal_reg, split_reg, LastWorkDay_Fri = LastWorkDay_Fri))

# >>> NUOVO (v6): controllo di sicurezza, nessun regressore deve essere tutto a zero
zero_cols <- colnames(cal_reg)[colSums(cal_reg != 0) == 0]
if (length(zero_cols) > 0) stop(paste("Regressori sempre a zero:", paste(zero_cols, collapse = ", ")))

print("Regressori di calendario nel modello:")
print(colnames(cal_reg))
print("Numero di giorni in cui ciascun regressore è attivo (deve essere > 0 per tutti):")
print(colSums(cal_reg != 0))


######################## pre-processing with time varying calendar effect
# >>> MODIFICA: eseguito SOLO se joint_calendar = FALSE (vecchio approccio in due passi)

if (!joint_calendar) {
  
  sm2<-model()
  eq2<-equation("eq")
  add(sm2,sarima("airline",c(7),c(0,1,1),c(0,1,1)))
  add(sm2, reg("cal",cal_reg,0.1))
  
  if(log==TRUE)
  {
    rslt2<-rjd3sts::estimate(sm2, 
                             log(data$Value),
                             marginal=F, 
                             initialization = "SqrtDiffuse",
                             optimizer="LevenbergMarquardt", 
                             concentrated = T, 
                             precision = 1e-10)
  }else{
    rslt2<-rjd3sts::estimate(sm2, 
                             data$Value, #log(data$Value),
                             marginal=F,
                             initialization = "SqrtDiffuse",
                             optimizer="LevenbergMarquardt", 
                             concentrated = T, 
                             precision = 1e-10)
  }
  
  fs<-result(rslt2,"ssf.filtered.states")
  ss<-result(rslt2,"ssf.smoothing.states")
  cmp<-result(rslt2,"ssf.smoothing.components")
  
  # Colonne di ss relative ai regressori: le prime 9 sono gli stati dell'airline settimanale
  cal_idx <- 10:(9 + ncol(cal_reg))
  
  coef_cal <- ss[nrow(ss), cal_idx]
  names(coef_cal) <- colnames(cal_reg)
  print("Coefficienti di calendario (fine campione):")
  print(round(if (log) exp(coef_cal) else coef_cal, 3))
  
  # Grafico degli effetti di calendario (dinamico)
  colors  <- hcl.colors(ncol(cal_reg), "Dark 3")
  y_range <- range(ss[, cal_idx], na.rm = TRUE)
  options(scipen = 999)
  layout(matrix(c(1,2), nrow = 2), heights = c(4, 2))
  
  plot(data$Date, ss[, cal_idx[1]], type = "l", col = colors[1],
       xlab = "Time", ylab = if(log) "Calendar effect (log-scale)" else "Calendar effect",
       ylim = y_range, main = "")
  for (k in 2:length(cal_idx)) lines(data$Date, ss[, cal_idx[k]], col = colors[k])
  
  mtext(paste0(label_original,": ","Calendar effects variation over time"), 
        side = 3, line = 1, font = 1, cex = 1.2, family = "sans")
  
  par(mar = c(0, 0, 0, 2))
  plot.new()
  legend("center",
         legend = colnames(cal_reg),
         col = colors, lty = 1, ncol = 4, cex = 0.7, bty = "n")
  layout(matrix(1))
  par(blank_par)
  
  if(log)
  {  
    data$Value_td_adjusted <- exp(log(data$Value) - cmp[,2])
    data$Time_Varying <- exp(cmp[,2])
  }else{
    data$Value_td_adjusted <- data$Value - cmp[,2]
    data$Time_Varying <- cmp[,2]
  }
}


#################### pre-processing

if (joint_calendar) {
  
  # >>> NUOVO: effetti di calendario stimati insieme al modello stagionale (7 e 365.2425)
  pre_pro <- fractionalAirlineEstimation(
    y = data$Value,
    x = cal_reg,
    log = log,
    criticalValue = 6,
    periods = c(7,365.2425),
    outliers = c("ao","wo", "ls"), y_time = data$Date)
  
  # >>> NUOVO: estrazione dell'effetto di calendario stimato.
  #     Prova prima il componente dei regressori utente; in alternativa X %*% b.
  #     In log restituisce un FATTORE (1 = nessun effetto); in livelli un effetto additivo (0).
  extract_calendar_effect <- function(pp, X, log) {
    n <- nrow(X); k <- ncol(X)
    comp <- pp$model$component_userdef_reg_variables
    if (!is.null(comp) && length(comp) == n) {
      comp <- as.numeric(comp)
      if (log) {
        # il componente può essere un fattore (neutro 1) o in scala log (neutro 0):
        # nella maggior parte dei giorni non c'è effetto, quindi la mediana dice quale dei due
        if (abs(median(comp) - 1) < abs(median(comp))) {
          return(list(eff = comp, how = "component_userdef_reg_variables (fattore)"))
        }
        return(list(eff = exp(comp), how = "component_userdef_reg_variables (log -> exp)"))
      }
      return(list(eff = comp, how = "component_userdef_reg_variables"))
    }
    b <- pp$model$b
    if (!is.null(b) && length(b) >= k) {
      e <- as.numeric(X %*% as.numeric(b[1:k]))
      return(list(eff = if (log) exp(e) else e, how = "X %*% b (primi k coefficienti)"))
    }
    warning("Impossibile estrarre l'effetto di calendario: la colonna SA non sarà valida")
    list(eff = rep(if (log) 1 else 0, n), how = "NON ESTRATTO")
  }
  
  cal_eff <- extract_calendar_effect(pre_pro, cal_reg, log)
  print(paste("Effetto di calendario estratto da:", cal_eff$how))
  
  # Il nome Time_Varying è mantenuto per compatibilità con il resto dello script,
  # anche se in questa modalità i coefficienti sono fissi.
  data$Time_Varying <- cal_eff$eff
  data$Value_td_adjusted <- if (log) data$Value / data$Time_Varying else data$Value - data$Time_Varying
  
  # >>> NUOVO: VERIFICA AUTOMATICA dell'estrazione.
  #     Serie osservata - linearizzata - calendario = solo effetti degli outlier.
  #     Gli outlier cambiano il valore solo in pochi giorni, quindi le differenze
  #     giornaliere del resto devono essere zero quasi ovunque.
  if (log) {
    remainder <- log(data$Value) - pre_pro$model$linearized - log(data$Time_Varying)
    tol <- 1e-6
  } else {
    remainder <- data$Value - pre_pro$model$linearized - data$Time_Varying
    tol <- 1e-6 * median(abs(data$Value))
  }
  share_ok <- mean(abs(diff(remainder)) < tol)
  print(paste0("Verifica estrazione calendario: ", round(100 * share_ok, 1),
               "% dei giorni coerenti (atteso > 95%)"))
  
  # Coefficienti stimati (se disponibili)
  if (!is.null(pre_pro$model$b) && length(pre_pro$model$b) >= ncol(cal_reg)) {
    coef_cal <- as.numeric(pre_pro$model$b[1:ncol(cal_reg)])
    names(coef_cal) <- colnames(cal_reg)
    print("Coefficienti di calendario (in log: fattori moltiplicativi):")
    print(round(if (log) exp(coef_cal) else coef_cal, 3))
  }
  
  # Struttura dell'output (da incollare in chat per controllo)
  str(pre_pro$model, max.level = 1)
  
} else {
  
  pre_pro <- fractionalAirlineEstimation(
    y = data$Value_td_adjusted,
    #x = TLS_regr, 
    log = log,
    criticalValue = 6,
    periods = c(7,365.2425), # weekly frequency
    outliers = c("ao","wo", "ls"), y_time = data$Date) 
}

print(pre_pro)

par(blank_par)
plot(pre_pro, main = label_original)


ls_presents <- (!is.null(pre_pro$model$component_ls) && 
                  length(pre_pro$model$component_ls) > 0 && 
                  !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls)))

################################################################################
########################## AMB #################################################
################################################################################
########################### decomposition #################################
######################## sequential approach # AMB seq

lin_series <- pre_pro$model$linearized


amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
  y = lin_series, # linearized series from preprocessing
  period = 7,
  #log=log, # log already done in lin_series
  y_time = data$Date) 


if(log)
{
  if (!grepl("\\(log-scale\\)", label)) 
  {
    label <- paste0(label_original," (log-scale)") 
  }  
}

options(scipen = 999)
print(amb.dow)

par(blank_par)
plot(amb.dow, main="sequential approach, s=7")
mtext(label, side = 2, line = 2.5)


# >>> NUOVO: componente stagionale intra-mensile (periodo medio del mese = 30.436875 giorni).
#     Cattura le forme graduali (es. la rampa della settimana prima di fine mese);
#     i picchi netti di singoli giorni restano affidati ai regressori di calendario.
if (use_dom_step) {
  amb.dom <- rjd3highfreq::fractionalAirlineDecomposition(
    y = amb.dow$decomposition$sa, # DOW-adjusted linearized data
    period = 30.436875,
    y_time = data$Date)
  
  print(amb.dom)
  par(blank_par)
  plot(amb.dom, main = "sequential approach, s=30.436875")
  mtext(label, side = 2, line = 2.5)
  
  input_doy <- amb.dom$decomposition$sa
} else {
  input_doy <- amb.dow$decomposition$sa
}


# Extract day-of-year pattern
# >>> MODIFICA: l'input è input_doy (DOW-adjusted, e anche DOM-adjusted se use_dom_step = TRUE)
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
  y = input_doy,
  period = 365.2425, # day of year pattern
  #log = log, # log data (if analysis performed in log scale) already produced by amb.dow$decomposition$sa
  y_time = data$Date)

if (ls_presents) {
  if (log) {
    # component_ls è un rapporto moltiplicativo in scala di livello (neutro = 1).
    # sa è ancora in log-scale a questo punto (l'exp() finale arriva dopo),
    # quindi si somma il LOG del rapporto: exp(sa + log(ls)) = exp(sa) * ls.
    # log(component_ls) = 0 quando non c'è LS, quindi il caso "nessun LS" è
    # automaticamente un no-op.
    amb.doy$decomposition$sa <- amb.doy$decomposition$sa + log(pre_pro$model$component_ls)
  } else {
    amb.doy$decomposition$sa <- amb.doy$decomposition$sa + pre_pro$model$component_ls
  }
}

par(blank_par)
options(scipen = 999)
plot(amb.doy, main="sequential approach, s=365.2425")
mtext(label, side = 2, line = 2.5)
print(amb.doy)


if(log==TRUE)
{
  amb.doy$decomposition$sa <- exp(amb.doy$decomposition$sa)
}  


par(blank_par)
layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
par(mar = c(5, 4, 4, 2))
plot(amb.dow$decomposition$y_time, data$Value,
     type = "l", col = "#F1C232",
     xlab = "Time", ylab = "")
lines(amb.dow$decomposition$y_time, amb.doy$decomposition$sa, col = "#006400")
title(main = "Sequential AMB", font.main = 1)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = c("Raw", "SA"),
       col = c("#F1C232", "#006400"),
       lty = 1, bty = "n", cex = 1.1)
par(blank_par)



# dow vs doy
y_vals <- c(amb.dow$decomposition$s, amb.doy$decomposition$s)
y_min <- min(y_vals)
y_max <- max(y_vals)
margin <- 0.025 * (y_max - y_min)  

par(blank_par)
layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
par(mar = c(5, 4, 4, 2))
plot(amb.dow$decomposition$y_time, amb.dow$decomposition$s,
     type = "l", col = "#FC8D62",
     xlab = "Time", ylab = "",
     ylim = c(y_min - margin, y_max + margin))  # <- qui estendi l’asse y
lines(amb.dow$decomposition$y_time, amb.doy$decomposition$s, col = "blue")
title(ylab = label)
title(main = "Sequential AMB", font.main = 1)
par(mar = c(0, 0, 0, 0))
plot.new()
# >>> MODIFICA: colori della legenda corretti (prima DOW e DOY erano invertiti)
legend("center",
       legend = c("DOW", "DOY"),
       col = c("#FC8D62", "blue"),
       lty = 1, bty = "n", cex = 1.1)
par(blank_par)


################################################################################
#################                S-I Ratio                    ##################
################################################################################

out_amb_seq  <- compute_si_daily(log = FALSE, s7 = amb.dow$decomposition$s, i = amb.doy$decomposition$i, dates = data$Date)
si_amb_seq   <- plot_si_weekly(dates = out_amb_seq$daily$date,    si = out_amb_seq$daily$si,   s7 = amb.dow$decomposition$s,        log_ratio = FALSE, show = "points")


######################### Decomposition diagnostics ############################

######################## seasonality tests on RAW series
# seasonality_canovahansen(data$Value, 365.2425, type = "Trigonometric") #15.11397
# seasonality_canovahansen(data$Value, 7, type = "Trigonometric") #1117.699
seasonality_f(data$Value, period=7)
seasonality_f(data$Value, period=365.2425)
seasonality_kruskalwallis(data$Value, period=7)
seasonality_kruskalwallis(data$Value, period=365.2425)
seasonality_periodogram(data$Value, period=365.2425)
seasonality_periodogram(data$Value, period=7)
seasonality_qs(data$Value, period=7)
seasonality_qs(data$Value, period=365.2425)

# f test
seasonality_f(amb.doy$decomposition$sa, period=7)
seasonality_f(amb.doy$decomposition$sa, period=365.2425)

#kruskall wallis
seasonality_kruskalwallis(amb.doy$decomposition$sa, period=7)
seasonality_kruskalwallis(amb.doy$decomposition$sa, period=365.2425)


# test of peaks at seasonal frequencies
seasonality_periodogram(amb.doy$decomposition$sa, period=365.2425)
seasonality_periodogram(amb.doy$decomposition$sa, period=7)

# Thresholds for p-values: p.9=2.49, p.95=3.83, p.99=7.06, p.999=11.88
seasonality_qs(amb.doy$decomposition$sa, period=7)
seasonality_qs(amb.doy$decomposition$sa, period=365.2425)




######################## spectral diagnostics (periodogram)

# sequential approach

par(blank_par)
layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
par(mar = c(5, 4, 4, 2))
periodogram_raw_data    <- spec.pgram(data$Value, log = FALSE, plot = FALSE) 
periodogram_ambSeq      <- spec.pgram(amb.doy$decomposition$sa, log = FALSE, plot = FALSE)
plot(periodogram_raw_data$freq, periodogram_raw_data$spec, type = "l", 
     col = "blue", lwd = 2,
     xlab = "Frequency", ylab = "Spectral density")
lines(periodogram_ambSeq$freq, periodogram_ambSeq$spec, col = "red", lwd = 2)
abline(v = c(1/7, 2/7, 3/7), col = "gray", lty = 2)              # Settimanale
abline(v = c(1/365.2425, 2/365.2425, 3/365.2425), col = "gray", lty = 4)  # Annuale
abline(v = freq_monthly, col = "darkred", lty = 3)  # Monthly


title(main = "Sequential AMB", font.main = 1)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = c("Raw", "SA"),
       col = c("blue", "red"),
       lty = 1, lwd = 2,
       bty = "n", cex = 1.1)
par(blank_par)




######################## irregular component analysis 

# AMB SEQUENTIAL approach
results_ambSeq_irr <- analyze_irregular(
  amb.doy$decomposition$i,
  name = "AMB Sequential"
)

plot(results_ambSeq_irr$acf)
plot(results_ambSeq_irr$pacf)


standardize <- function(x) {
  x <- x[!is.na(x)]
  (x - mean(x)) / sd(x)
}

irr_ambSeq_std   <- standardize(amb.doy$decomposition$i)

plot_hist_panel(amb.doy$decomposition$i, irr_ambSeq_std, "AMB seq")
par(blank_par)




################################################################################
############################# Revisions analysis ###############################
################################################################################

# 
# amb_estimator2 <- AMBEstimator$new(data = data, dates_from_pre_pro = date_start, log_pre_pro = log)
# results_amb2 <- calculate_revisions_by_lags( estimator_instance = amb_estimator2, lags = c(7, 30, 90))
# results_amb2

################################################################################
######################### KDE analysis #########################################
################################################################################

ts_raw_data <- with(data[order(data$Date), ],
                    ts(Value,
                       start = c(as.numeric(format(min(Date), "%Y")),
                                 as.numeric(format(min(Date), "%j"))),
                       frequency = 365))

ts_sa_AMB_seq <- ts(
  amb.doy$decomposition$sa,
  start = c(
    as.numeric(format(min(amb.doy$decomposition$y_time), "%Y")),
    as.numeric(format(min(amb.doy$decomposition$y_time), "%j"))
  ),
  frequency = 365
)



xlim_range <- c(-0.5, 0.5) # adjust them basing on the plot ranges of these data
ylim_range <- c(0, 7.5)

# Tutto insieme
# KDE_AMB_seq   <- plot_kde_acf(raw_data = ts_raw_data, sa_data = ts_sa_AMB_seq  , lags = c(7, 30,365), window_cycles =  2, xlim_min = xlim_range[1], xlim_max = xlim_range[2], ylim_min = ylim_range[1], ylim_max = ylim_range[2], show_legend = TRUE, title_label = "KDE of sample ACF for AMB sequential")
# KDE_AMB_seq

KDE_AMB_seq_7_365   <- plot_kde_acf(raw_data = ts_raw_data, sa_data = ts_sa_AMB_seq  , lags = c(7, 365), window_cycles =  c(4,2), xlim_min = xlim_range[1], xlim_max = xlim_range[2], ylim_min = ylim_range[1], ylim_max = ylim_range[2], show_legend = TRUE, title_label = "KDE of rolling ACF for AMB sequential")
KDE_AMB_seq_7_365

KDE_AMB_seq_30   <- plot_kde_acf(raw_data = ts_raw_data, sa_data = ts_sa_AMB_seq  , lags = c(30), window_cycles =  c(3), xlim_min = xlim_range[1], xlim_max = xlim_range[2], ylim_min = ylim_range[1], ylim_max = ylim_range[2], show_legend = TRUE, title_label = "KDE of rolling ACF for AMB sequential")
KDE_AMB_seq_30
################################################################################
######################### MONTHLY SA_CA COMPARISON  ############################
################################################################################


# ---------------------------------------------------------------------------
# Parametri periodo di interesse (uguali per tutti i metodi)
# ---------------------------------------------------------------------------

year_start   <- "2023-01-01"
year_end     <- "2023-12-31"
sa_end_extra <- "2024-01-31"

Sys.setlocale("LC_TIME", "C")

# ---------------------------------------------------------------------------
# Aggregazione giornaliera -> mensile e destagionalizzazione con i 4 metodi
# ---------------------------------------------------------------------------

res <- aggregate_daily_to_monthly(data, fun = mean)

SA_full  <- sa_all_methods(res)
SA       <- SA_full %>% filter_range("date", year_start, sa_end_extra)

# Serie raw giornaliera ristretta all'anno di interesse (sfondo grigio nei grafici)
data2 <- filter_range(data, "Date", year_start, year_end)

# ---------------------------------------------------------------------------
# AMB sequential
# ---------------------------------------------------------------------------
plot_sa_ca_comparison(
  sa_vector    = amb.doy$decomposition$sa,
  sa_column    = "TRAMO_SEATS",
  title        = "Sequential AMB",
  legend_label = "SA monthly (TRAMO-SEATS)",
  log = log,
  series = "SA" 
  # series = "SA_CA" # per non avere i picchi stagionali
)



################################################################################
#                              Produce Outputs                                 #
################################################################################

sa_outputs <- produce_outputs(
  data = data,
  sa_daily = amb.doy$decomposition$sa,
  calendar_effect = data$Time_Varying
)

raw_monthly   <- sa_outputs$raw_monthly
sa_daily      <- sa_outputs$sa_daily
sa_ca_monthly <- sa_outputs$sa_ca_monthly




# -----------------------------
# DAILY
# SA: effetti di calendario riaggiunti alla SA_CA giornaliera
# -----------------------------

# >>> MODIFICA: in log l'effetto di calendario è un fattore e va moltiplicato, non sommato
daily <- data.frame(
  Date = data$Date,
  Raw = data$Value,
  SA_CA = amb.doy$decomposition$sa,
  SA = if (log) amb.doy$decomposition$sa * data$Time_Varying else amb.doy$decomposition$sa + data$Time_Varying
)


# -----------------------------
# MONTHLY
# -----------------------------

# Raw giornaliero aggregato mensilmente
raw_monthly <- data %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarise(
    Raw_monthly = mean(Value, na.rm = TRUE)
  )

# SA + calendario giornaliera aggregata mensilmente
sa_ca_monthly <- data.frame(
  Date = data$Date,
  SA_CA = amb.doy$decomposition$sa
) %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarise(
    SA_CA_monthly = mean(SA_CA, na.rm = TRUE)
  )

# SA giornaliera aggregata mensilmente
# >>> MODIFICA: stessa correzione (moltiplicare in log)
sa_monthly <- data.frame(
  Date = data$Date,
  SA = if (log) amb.doy$decomposition$sa * data$Time_Varying else amb.doy$decomposition$sa + data$Time_Varying
) %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarise(
    SA_monthly = mean(SA, na.rm = TRUE)
  )


# RSAfull: aggregazione giornaliera -> mensile -> TRAMO-SEATS
rsa_monthly <- data.frame(
  Date = seq(
    from = floor_date(min(data$Date), "month"),
    by = "month",
    length.out = length(SA_full$TRAMO_SEATS)
  ),
  RSAfull_TRAMO_SEATS = as.numeric(SA_full$TRAMO_SEATS)
)


monthly <- raw_monthly %>%
  left_join(sa_monthly, by = "month") %>%
  left_join(sa_ca_monthly, by = "month") %>%
  left_join(rsa_monthly, by = c("month" = "Date")) %>%
  rename(Date = month)


################################################################################
#        >>> NUOVO: Diagnostica della stagionalità intra-mensile residua       #
################################################################################
# Per ogni giorno: SA_CA / mediana della SA_CA del proprio mese.
# Se la destagionalizzazione è corretta, i profili devono essere piatti intorno a 1.

diag_prof <- daily %>%
  mutate(m      = floor_date(Date, "month"),
         dom    = day(Date),
         to_eom = as.integer(days_in_month(Date)) - day(Date)) %>%
  group_by(m) %>%
  mutate(rel = SA_CA / median(SA_CA)) %>%
  ungroup()

# 1) Profilo per giorno del mese (1..31)
prof_dom <- diag_prof %>% group_by(dom) %>% summarise(med = median(rel))
print(prof_dom, n = 31)

# 2) Profilo per giorni mancanti alla fine del mese (0 = ultimo giorno)
prof_eom <- diag_prof %>% dplyr::filter(to_eom <= 9) %>%
  group_by(to_eom) %>% summarise(med = median(rel))
print(prof_eom)

# 3) Giorni con regressore dedicato, separati per feriale / sabato / domenica
#    (tutti i valori dovrebbero essere vicini a 1)
chk <- diag_prof %>%
  mutate(g = case_when(wday(Date, week_start = 1) == 7 ~ "Sun",
                       wday(Date, week_start = 1) == 6 ~ "Sat",
                       TRUE ~ "Weekday"),
         tipo = case_when(dom == 1 ~ "Day1", dom == 15 ~ "Day15", dom == 20 ~ "Day20",
                          to_eom == 0 ~ "EoM", to_eom == 1 ~ "EoM-1",
                          to_eom == 2 ~ "EoM-2", to_eom == 3 ~ "EoM-3",
                          to_eom == 4 ~ "EoM-4", to_eom == 5 ~ "EoM-5")) %>%
  dplyr::filter(!is.na(tipo)) %>%
  group_by(tipo, g) %>%
  summarise(med = median(rel), n = n(), .groups = "drop")
print(chk, n = 30)

# 4) >>> NUOVO (v3): EoM-1 ed EoM-2 feriali, venerdì vs altri giorni
#    (dopo il regressore LastWorkDay_Fri entrambi dovrebbero essere vicini a 1)
chk_fri <- diag_prof %>%
  dplyr::filter(to_eom %in% 1:2, wday(Date, week_start = 1) <= 5) %>%
  mutate(venerdi = wday(Date, week_start = 1) == 5) %>%
  group_by(to_eom, venerdi) %>%
  summarise(med = median(rel), n = n(), .groups = "drop")
print(chk_fri)

# 5) >>> NUOVO (v4): riepilogo della verifica sull'effetto di calendario
#    (ristampato qui per comodità; serve a sapere se la colonna SA è corretta)
if (joint_calendar) {
  cat("\n================ VERIFICA CALENDARIO ================\n")
  cat("Regressori di calendario nel modello:", ncol(cal_reg), "\n")
  cat("Effetto di calendario estratto da:", cal_eff$how, "\n")
  cat("Giorni coerenti:", round(100 * share_ok, 1), "% (atteso > 95%)\n")
  cat("=====================================================\n\n")
}

# Grafico (da usare anche nel paper)
plot_prof_dom <- ggplot(diag_prof, aes(factor(dom), rel)) +
  geom_boxplot(fill = "grey80", color = "black", outlier.size = 0.5) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = "Day of month", y = "SA_CA / monthly median",
       title = "Residual intra-month pattern in SA_CA") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5))
plot_prof_dom


# -----------------------------
# EXPORT EXCEL
# -----------------------------

write.xlsx(
  list(
    Daily = daily,
    Monthly = monthly
  ),
  file = "seasonal_adjustment_outputs_Payments__LOG_SUM.xlsx",
  overwrite = TRUE
)


################################################################################
#                               Save workspace                                 #
################################################################################

save.image(file="workspace_electronic_payments_LOG_SUM.RData")














################################################################################
#                                                                              #
#   APPENDICE - DMR CON UN CALENDARIO BASE (figura per l'abstract)             #
#                                                                              #
################################################################################
# Da accodare allo script finale della fatturazione e lanciare DOPO lo script
# completo: usa gli oggetti data, log, italian_calendar e la funzione
# generate_eom_regressors (utilities.R).
# Non modifica nessun oggetto dello script principale: tutto ciò che viene
# creato qui ha il suffisso _base.
#
# Calendario base:
# - 12 festività nazionali nella costruzione predefinita del pacchetto
#   (type = "All", quindi attive anche quando cadono nel weekend);
# - 3 dummy di fine mese (EoM, EoM-1, EoM-2), senza distinzione per tipo di giorno.
# Stima congiunta con il modello stagionale e decomposizione AMB identiche
# alla versione finale: cambia SOLO il calendario.
#
# Output:
# - stampa in console delle mediane del DMR per i giorni citati nel testo
#   (giorni 1, 15, 20 e ultimi giorni del mese), per controllare i numeri;
# - grafici del DMR per giorno del mese, calendario base e finale, nello
#   stesso stile e sulla stessa scala, salvati in PNG a 300 dpi.
################################################################################

# ------------------------------------------------------------------------------
# 1) Regressori del calendario base
# ------------------------------------------------------------------------------
cal_base <- rjd3toolkit::holidays(
  calendar = italian_calendar,
  start = min(data$Date), length = nrow(data),
  type = "All", nonworking = c(6, 7))

eom_base <- generate_eom_regressors(data$Date, num_days_before = 2)   # EoM, EoM-1, EoM-2

X_base <- as.matrix(cbind(cal_base, eom_base))
colnames(X_base) <- c(paste0("holiday_", seq_len(ncol(cal_base))),
                      paste0("eom_minus_", seq_len(ncol(eom_base)) - 1))

# Controlli: nessuna colonna sempre a zero, matrice a rango pieno
stopifnot(all(colSums(X_base != 0) > 0), qr(X_base)$rank == ncol(X_base))

# ------------------------------------------------------------------------------
# 2) Pre-trattamento con il calendario base (stima congiunta + outlier)
# ------------------------------------------------------------------------------
pre_pro_base <- fractionalAirlineEstimation(
  y = data$Value, x = X_base, log = log, criticalValue = 6,
  periods = c(7, 365.2425), outliers = c("ao", "wo", "ls"), y_time = data$Date)

# ------------------------------------------------------------------------------
# 3) Decomposizione AMB sequenziale (settimanale, poi annuale)
# ------------------------------------------------------------------------------
dow_base <- rjd3highfreq::fractionalAirlineDecomposition(
  y = pre_pro_base$model$linearized, period = 7, y_time = data$Date)
doy_base <- rjd3highfreq::fractionalAirlineDecomposition(
  y = dow_base$decomposition$sa, period = 365.2425, y_time = data$Date)

sa_ca_base <- doy_base$decomposition$sa

# Reinserimento dei cambi di livello (stessa logica dello script principale:
# component_ls è un rapporto in scala di livello, neutro = 1)
ls_base <- pre_pro_base$model$component_ls
if (!is.null(ls_base) && length(ls_base) == length(sa_ca_base) && !all(is.na(ls_base))) {
  sa_ca_base <- if (log) sa_ca_base + log(ls_base) else sa_ca_base + ls_base
}
if (log) sa_ca_base <- exp(sa_ca_base)

# ------------------------------------------------------------------------------
# 4) DMR: SA_CA del giorno / mediana della SA_CA del suo mese
# ------------------------------------------------------------------------------
dmr_base <- data.frame(Date = data$Date, SA_CA = sa_ca_base) %>%
  mutate(month  = floor_date(Date, "month"),
         dom    = day(Date),
         to_eom = as.integer(days_in_month(Date)) - day(Date)) %>%
  group_by(month) %>%
  mutate(DMR = SA_CA / median(SA_CA)) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 5) Mediane da confrontare con i numeri citati nel testo dell'abstract
# ------------------------------------------------------------------------------
cat("\nDMR medians with the base calendar - fixed days of the month:\n")
print(dmr_base %>%
        dplyr::filter(dom %in% c(1, 15, 20)) %>%
        group_by(day_of_month = dom) %>%
        summarise(median_DMR = round(median(DMR), 3), n = n()))

cat("\nDMR medians with the base calendar - days to month end (0 = last day):\n")
print(dmr_base %>%
        dplyr::filter(to_eom <= 7) %>%
        group_by(days_to_month_end = to_eom) %>%
        summarise(median_DMR = round(median(DMR), 3), n = n()))

# ------------------------------------------------------------------------------
# 6) Grafici: calendario base e calendario finale, nello STESSO stile e con la
#    STESSA scala dell'asse y, così sono confrontabili a colpo d'occhio
# ------------------------------------------------------------------------------
# Boxplot standard: scatola = distanza interquartile, linea = mediana,
# baffi = 1.5 x IQR, punti = valori oltre i baffi. Linea tratteggiata = 1.
# Il DMR del calendario finale è la colonna rel di diag_prof (script principale).

plot_dmr <- function(df, yvar, title_txt, ylim_common) {
  ggplot(df, aes(x = factor(dom), y = .data[[yvar]])) +
    geom_boxplot(fill = "grey80", color = "black", outlier.size = 0.5) +
    geom_hline(yintercept = 1, lty = 2) +
    coord_cartesian(ylim = ylim_common) +
    labs(x = "Day of month",
         y = "Day to Month Ratio = SCA / monthly median",
         title = title_txt) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),
          plot.background = element_blank(),
          plot.title = element_text(hjust = 0.5))
}

# Scala comune ai due grafici (i valori oltre 2.5 restano nel calcolo dei box,
# ma non vengono disegnati).
# Limite superiore fissato a 2.5; limite inferiore dal minimo dei due grafici
ylim_common <- c(min(c(dmr_base$DMR, diag_prof$rel), na.rm = TRUE), 2.5)

plot_dmr_base <- plot_dmr(dmr_base, "DMR",
                          "E-invoicing: residual intra-month pattern in SCA series with a basic calendar",
                          ylim_common)
plot_dmr_final <- plot_dmr(diag_prof, "rel",
                           "E-invoicing: residual intra-month pattern in SCA series with the final calendar",
                           ylim_common)
plot_dmr_base
plot_dmr_final

# Salvataggio: 16 x 7 cm (larghezza del testo del modello), 300 dpi
ggsave("DMR_einvoicing_basic_calendar.png", plot_dmr_base,
       width = 16, height = 7, units = "cm", dpi = 300)
ggsave("DMR_einvoicing_final_calendar.png", plot_dmr_final,
       width = 16, height = 7, units = "cm", dpi = 300)

# ------------------------------------------------------------------------------
# 7) Figura combinata: calendario base sopra, calendario finale sotto,
#    stessa scala dell'asse y (un unico grafico a pannelli, senza pacchetti extra)
# ------------------------------------------------------------------------------
dmr_both <- bind_rows(
  dmr_base   %>% transmute(dom, DMR,       calendar = "Basic calendar"),
  diag_prof  %>% transmute(dom, DMR = rel, calendar = "Final calendar")
) %>%
  mutate(calendar = factor(calendar, levels = c("Basic calendar", "Final calendar")))

plot_dmr_both <- ggplot(dmr_both, aes(x = factor(dom), y = DMR)) +
  geom_boxplot(fill = "grey80", color = "black", outlier.size = 0.5) +
  geom_hline(yintercept = 1, lty = 2) +
  facet_wrap(~ calendar, ncol = 1) +
  coord_cartesian(ylim = ylim_common) +
  labs(x = "Day of month",
       y = "Day to Month Ratio = SCA / monthly median",
       title = "E-invoicing: residual intra-month pattern in SCA series") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 10, hjust = 0.5))

plot_dmr_both

# Salvataggio: 16 x 13 cm (larghezza del testo del modello), 300 dpi
ggsave("DMR_einvoicing_basic_vs_final.png", plot_dmr_both,
       width = 16, height = 13, units = "cm", dpi = 300)
