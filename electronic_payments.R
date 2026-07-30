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

label          <- "Electronic payments"
label_original <- "Electronic payments"
lab <- label
date_start <- "2019-01-01"
date_end   <- "2025-12-31"

col_to_analyze <- "imposta" # Cambiarlo per analizzare certi settori specifici es. "sett00num_fatture"


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

KDE_raw   <- plot_kde_acf(raw_data = ts_raw_data, lags = c(7, 30, 365), window_cycles =  2, xlim_min = xlim_range[1], xlim_max = xlim_range[2], ylim_min = ylim_range[1], ylim_max = ylim_range[2], show_legend = TRUE, title_label = "KDE of sample ACF for raw data")
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
cal_reg <- rjd3toolkit::holidays(
  calendar = italian_calendar,   # Use the Italian calendar
  start = min(data$Date), length = nrow(data),
  type = "All", nonworking = c(6,7))

colnames(cal_reg) <- c("Liberation Day", "Republic Day", "Immaculate Conception", "St. Stephen's Day", "Epiphany",
                       "New Year’s Day", "Easter Monday", "Labour Day", "Assumption of Mary", "All Saints' Day", "Christmas", "Easter")


####### End of Month Effect

# 1. Estrarre le date dal dataset principale 
dates <- as.Date(data$Date)

# 3. Creare la matrice standalone (0/1) usando le date come nomi delle righe
eom_regressors <- generate_eom_regressors(data$Date, num_days_before = 2) 

# Append della nuova colonna alla matrice esistente
cal_reg <- cbind(cal_reg, eom_regressors)

######################## estimated calendar effects

# pre_pro <- fractionalAirlineEstimation(
#   y = data$Value,
#   x = cal_reg,
#   log = log,
#   criticalValue = 6 ,
#   periods = c(7,365.2425), # weekly and yearly frequency
#   outliers = c("ao","wo", "ls"), y_time = data$Date) 
# 
# print(pre_pro)
# 
# par(blank_par)
# plot(pre_pro)
# 


######################## pre-processing with time varying calendar effect

library(rjd3filters)
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
                           precision = 1e-10, initialParameters = )
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



colors <- c("#E41A1C", 
            "#377EB8", 
            "#4DAF4A", 
            "#984EA3", 
            "#FF7F00", 
            "#FFFF33", 
            "#A65628", 
            "#F781BF", 
            "#999999", 
            "#66C2A5", 
            "#FC8D62", 
            "#8DA0CB",
            "#053061", "#2166AC", "#4393C3") 

y_range <- range(ss[,10:24], na.rm = TRUE)
options(scipen = 999)
layout(matrix(c(1,2), nrow = 2), heights = c(4, 1))

# grafico
plot(data$Date, ss[,10], type = "l", col = colors[1],
     xlab = "Time", ylab = if(log) "Calendar effect (log-scale)" else "Calendar effect",
     ylim = y_range, main = "")

for (i in 11:24) lines(data$Date, ss[,i], col = colors[i - 9])

mtext(paste0(label_original,": ","Calendar effects variation over time"), 
      side = 3, line = 1, font = 1, cex = 1.2, family = "sans")

# legenda sotto
par(mar = c(0, 0, 0, 2))
plot.new()
legend("center",
       legend = c("Liberation Day", "Republic Day", "Immaculate Conception", "St. Stephen's Day", "Epiphany",
                  "New Year’s Day", "Easter Monday", "Labour Day", "Assumption of Mary", "All Saints' Day", "Christmas", "Easter", "EoM", "EoM-1", "EoM-2"),
       col = colors, lty = 1, ncol = 3, cex = 0.9, bty = "n")
layout(matrix(1))

par(blank_par) # Reload original plot window settings




if(log)
{  
  data$Value_td_adjusted <- exp(log(data$Value) - cmp[,2])
  data$Time_Varying <- exp(cmp[,2])
  
}else{
  data$Value_td_adjusted <- data$Value - cmp[,2]
  data$Time_Varying <- cmp[,2]
}


#################### pre-processing

pre_pro <- fractionalAirlineEstimation(
  y = data$Value_td_adjusted,
  #x = TLS_regr, 
  log = log,
  criticalValue = 6,
  
  periods = c(7,365.2425), # weekly frequency
  outliers = c("ao","wo", "ls"), y_time = data$Date) 

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
mtext(label, side = 2, line = 2.5)
print(amb.dow)

par(blank_par)
plot(amb.dow, main="sequential approach, s=7")
mtext(label, side = 2, line = 2.5)

# Extract day-of-year pattern from day-of-week-adjusted linearized data
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
  y = amb.dow$decomposition$sa, # DOW-adjusted linearized data
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
legend("center",
       legend = c("DOW", "DOY"),
       col = c("blue", "#FC8D62"),
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

KDE_AMB_seq_7_365   <- plot_kde_acf(raw_data = ts_raw_data, sa_data = ts_sa_AMB_seq  , lags = c(7, 365), window_cycles =  2, xlim_min = xlim_range[1], xlim_max = xlim_range[2], ylim_min = ylim_range[1], ylim_max = ylim_range[2], show_legend = TRUE, title_label = "KDE of sample ACF for AMB sequential")
KDE_AMB_seq_7_365

KDE_AMB_seq_30   <- plot_kde_acf(raw_data = ts_raw_data, sa_data = ts_sa_AMB_seq  , lags = c(30), window_cycles =  2, xlim_min = xlim_range[1], xlim_max = xlim_range[2], ylim_min = ylim_range[1], ylim_max = ylim_range[2], show_legend = TRUE, title_label = "KDE of sample ACF for AMB sequential")
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
# RSAfull: Time varying calendar effects riaggiunti alla SA giornaliera
# -----------------------------

daily <- data.frame(
  Date = data$Date,
  Raw = data$Value,
  SA_CA = amb.doy$decomposition$sa,
  SA = amb.doy$decomposition$sa + data$Time_Varying
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
sa_monthly <- data.frame(
  Date = data$Date,
  SA = amb.doy$decomposition$sa + data$Time_Varying
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




