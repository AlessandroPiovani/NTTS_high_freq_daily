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
library(rjd3tramoseats)
library(rjd3sts)
library(rjd3x11plus)
library(rjd3stl)
library(rjd3x13)

# Input definitions 

base_path                 <- "C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\NTTS_high_freq_git\\NTTS_high_freq"
# base_path                 <- "C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\high_frequency_daily"

# input_data_path           <- paste0(base_path,"\\data\\output_serie_storica_SUM.csv")
input_data_path           <- paste0(base_path,"\\data\\output_serie_storica_IT.csv")

monthly_SA_benchmark_path <- paste0(base_path,"\\code\\monthly_to_compare\\electricity_demand_monthly_SA.xlsx")
setwd(paste0(base_path,"\\code"))

blank_par <- par(no.readonly = TRUE)   # salva lo stato originale della finestra grafica per ripristinarlo dopo che faccio grigli edi grafici



# load RData
# load("workspace_electricity_demand_north.RData")

# source
source("utilities.R")
source("revision.R")
source("stability.R")

label          <- "Electronic payments"
label_original <- "Electronic payments"
lab <- label
date_start <- "2019-01-01"
date_end   <- "2025-12-31"


# import data
data <- read.csv(input_data_path, sep=";")

colnames(data)[colnames(data) == "data"] <- "Date"
data$Value <- data[["imponibile"]] # seasonal adjustment of Imponibile

# restrict time period
data = data %>% 
  dplyr::filter(Date >= as.Date(date_start))%>% 
  dplyr::filter(Date <= as.Date(date_end)) 
data$Date <- as.Date(data$Date)


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
# 
# all_seas_tests_original_data

# Salvataggio dei risultati dei test, che ci mettono una vita
# output_rdata_path <- paste0(base_path, "\\data\\all_seas_tests_el_payments_data.RData")
# save(all_seas_tests_original_data, file = output_rdata_path)

# load(paste0(base_path, "\\data\\all_seas_tests_el_payments_data.RData"))
# print_seasonality_tests(all_seas_tests_original_data, alpha = 0.05)

# seasonality_plot <- plot_seasonal_spectrum(all_seas_tests_original_data)
# print(seasonality_plot)

# risultato_picchi <- analyze_spectral_peaks(data$Value, max_peaks = 15)
# print(risultato_picchi, row.names = FALSE)

######################## periodogram

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
ylim_range <- c(0, 10)

KDE_raw   <- plot_kde_acf(raw_data = ts_raw_data, lags = c(7, 30, 365), window_cycles =  2, xlim_min = xlim_range[1], xlim_max = xlim_range[2], ylim_min = ylim_range[1], ylim_max = ylim_range[2], show_legend = TRUE, title_label = "KDE of sample ACF for raw data")
KDE_raw


################################################################################
##############################  Log-level test #################################
################################################################################
# check all values are larger than zero
data_test <- data

# Calcoliamo il minimo globale della serie originale
min_val <- min(data_test$Value, na.rm = TRUE)

data_test <- data_test %>%
  mutate(Value = if (min_val <= 0) Value + abs(min_val) + 1 else Value)

data_test %>% 
  dplyr::filter(Value<=0)

# compare aicc of additive and multiplicative models
# jspec     <-  rjd3highfreq::.extended_airline_spec(periodicities = c(7),differencing = -1, ar = FALSE, toint = FALSE) #differencing=-1 -> auto differencing
jspec     <-  rjd3highfreq::.extended_airline_spec(periodicities = c(7,365.2425), differencing = -1, ar = FALSE, toint = FALSE) #differencing=-1 -> auto differencing
jregarima <-  rjd3highfreq::.extended_airline_regarima(y = data_test$Value, jspec=jspec)
aiccs     <-  rjd3highfreq::.extended_airline_loglevel(jregarima = jregarima,   jspec = jspec) # {ll.getAICcLevel(), ll.getAICcLog()}, in Java code

if(aiccs[1] <= aiccs[2]) {
 print("No log transformation applied")
  log <- FALSE
}  else {
  print("Log transformation should be applied")
  log <- TRUE
}

aiccs[1]
aiccs[2]


log <- FALSE # Considerando che gli AICC si somigliano molto, forzo NO LOG 
             # (anche perchè ci sono valori negativi e ho dovuto sommare una 
             # costante alla serie per renderli positivi)

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
# colnames(cal_reg) <- c("25th_apr", "2nd_jun", 
#                        "8th_dec", "26th_dec","epiphany","1st_jan", "easter_mon", "1st_may", "15th_aug", 
#                        "1st_nov", "xmas", "easter" )

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

pre_pro_no_time_varying_cal_eff <- fractionalAirlineEstimation(
  y = data$Value,
  x = cal_reg,
  criticalValue = 6 ,
  # periods = c(7, 29, 30, 31, 365), # weekly frequency
  periods = c(7, 365), # weekly frequency
  outliers = c("ao","wo","ls"), log = log, y_time = data$Date)
print(pre_pro_no_time_varying_cal_eff)



######################## pre-processing with time varying calendar effect
library(rjd3sts)
sm <- model()
eq <- equation("eq")
add(sm, sarima("airline", 7, c(0,1,1),c(0,1,1)))
add(sm, reg("cal", cal_reg, 0.1))
rslt <- rjd3sts::estimate(sm, data$Value, marginal = F, initialization = "SqrtDiffuse",
                 optimizer = "LevenbergMarquardt",
                 concentrated = TRUE, precision = 1e-10)
# rslt_log <- estimate(sm, data$Log_Value, marginal = F, initialization = "SqrtDiffuse", optimizer = "LevenbergMarquardt", concentrated = TRUE, precision = 1e-10)
ss   <- rjd3sts::smoothed_states(rslt)
sc   <- rjd3sts::smoothed_components(rslt)
# sc   <- rjd3sts::smoothed_components(rslt_log)
dates <- seq.Date(from = as.Date(date_start), by = "day", length.out = nrow(sc))
rownames(sc) <- as.character(dates)

data$Time_Varying <- sc[,2]
data$Value_td_adjusted <- data$Value - sc[,2]

if(log)
{  
  data$Value_td_adjusted <- data$Value/exp(cmp[,2])
}else{
  data$Value_td_adjusted <- data$Value - cmp[,2]
}

library(rjd3filters)
sm2<-model()
eq2<-equation("eq")
add(sm2,sarima("airline",7,c(0,1,1),c(0,1,1)))
# add(sm2,sarima("airline",365,c(0,1,1),c(0,1,1)))
add(sm2, reg("cal",cal_reg,0.1))

rslt2<-rjd3sts::estimate(sm2, 
               data$Value, #log(data$Value),
               marginal=F,
               initialization = "SqrtDiffuse",
               optimizer="LevenbergMarquardt", 
               concentrated = T, 
               precision = 1e-10)
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

#################### pre-processing

pre_pro <- fractionalAirlineEstimation(
  y = data$Value_td_adjusted,
  criticalValue = 6 ,
  periods = c(7, 365.2425), # weekly frequency
  outliers = c("ao","wo", "ls"), log = log, y_time = data$Date) 
print(pre_pro)
plot(pre_pro, main = label)

pacf(pre_pro$model$residuals)
acf(pre_pro$model$residuals)



################################################################################
########################## AMB #################################################
################################################################################
########################### decomposition #################################

######################## sequential approach
# Decomposition with weekly pattern
amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
  y = pre_pro$model$linearized, # linearized series from preprocessing
  period = 7,
  log = log, y_time = data$Date)
options(scipen = 999)
plot(amb.dow, main="sequential approach, s=7")
mtext(label, side = 2, line = 2.5)
print(amb.dow)

# Extract day-of-year pattern from day-of-week-adjusted linearised data
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
  # y = amb.dow$decomposition$sa, # DOW-adjusted linearised data
  y = pre_pro$model$linearized-amb.dow$decomposition$s, # DOW-adjusted linearised data
  period = 365.2425, # day of year pattern
  log = log, y_time = data$Date)
options(scipen = 999)
plot(amb.doy, main="sequential approach, s=365.2425")
mtext(label, side = 2, line = 2.5)

# Re-add LS
if(!is.null(pre_pro$model$component_ls) && 
   length(pre_pro$model$component_ls) > 0 && 
   !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls)))
{
  if(log){
    amb.doy$decomposition$sa <- amb.doy$decomposition$sa + log(pre_pro$model$component_ls)
  }else{
    amb.doy$decomposition$sa <- amb.doy$decomposition$sa + pre_pro$model$component_ls
  }
}  

if(log)
{
  amb.doy$decomposition$sa <- exp( amb.doy$decomposition$sa)
}  

print(amb.doy)



layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
par(mar = c(5, 4, 4, 2))
plot(amb.dow$decomposition$y_time, data$Value,
     type = "l", col = "#F1C232",
     xlab = "Time", ylab = label)
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

layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
par(mar = c(5, 4, 4, 2))
plot(amb.dow$decomposition$y_time, amb.dow$decomposition$s,
     type = "l", col = "#FC8D62",
     xlab = "Time", ylab = label,
     ylim = c(y_min - margin, y_max + margin))  # <- qui estendi l’asse y
lines(amb.dow$decomposition$y_time, amb.doy$decomposition$s, col = "blue")
title(main = "Sequential AMB", font.main = 1)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = c("DOW", "DOY"),
       col = c("#FC8D62", "blue"),
       lty = 1, bty = "n", cex = 1.1)
par(blank_par)



######################## seasonality tests on SA series

# Lunghi da eseguire

# seasonality_canovahansen(data$Value, 365.2425, type = "Trigonometric") #15.11397
# seasonality_canovahansen(data$Value, 7, type = "Trigonometric") #1117.699
# seasonality_canovahansen(amb.doy$decomposition$sa, 365.2425, type = "Trigonometric") #8.458398
# seasonality_canovahansen(amb.doy$decomposition$sa, 7, type = "Trigonometric") #0.0004525432

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


######################## seasonality tests on irregular component
# seasonality_canovahansen(amb.doy$decomposition$i, 365.2425, type = "Trigonometric") #8.458398
# seasonality_canovahansen(amb.doy$decomposition$i, 7, type = "Trigonometric") #0.0004525432


# f test
seasonality_f(amb.doy$decomposition$i, period=7)
seasonality_f(amb.doy$decomposition$i, period=365.2425)


#kruskall wallis. 
seasonality_kruskalwallis(amb.doy$decomposition$i, period=7)
seasonality_kruskalwallis(amb.doy$decomposition$i, period=365.2425)


# h0: no seasonality
seasonality_periodogram(amb.doy$decomposition$i, period=365.2425)
seasonality_periodogram(amb.doy$decomposition$i, period=7)
# Thresholds for p-values: p.9=2.49, p.95=3.83, p.99=7.06, p.999=11.88

seasonality_qs(amb.doy$decomposition$i, period=7)
seasonality_qs(amb.doy$decomposition$i, period=365.2425)


######################## spectral diagnostics (periodogram)
# sequential approach 
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

layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
par(mar = c(5, 4, 4, 2))


periodogram_raw_data <- spec.pgram(data$Value,               log = FALSE, plot = FALSE)
periodogram_ambMulti <- spec.pgram(amb.doy$decomposition$sa, log = FALSE, plot = FALSE)

plot(periodogram_raw_data$freq, periodogram_raw_data$spec, type = "l", 
     col = "blue", lwd = 2,
     xlab = "Frequency", ylab = "Spectral density")
lines(periodogram_ambMulti$freq, periodogram_ambMulti$spec, col = "red", lwd = 2)
abline(v = c(1/7, 2/7, 3/7), col = "gray", lty = 2)              # Settimanale
abline(v = c(1/365.2425, 2/365.2425, 3/365.2425), col = "gray", lty = 4)  # Annuale
abline(v = freq_monthly, col = "darkred", lty = 3)  # Monthly
title(main = "Simultaneous AMB", font.main = 1)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = c("Raw", "SA"),
       col = c("blue", "red"),
       lty = 1, lwd = 2,
       bty = "n", cex = 1.1)
par(blank_par)


# sequential approach 
# periodogrm_ambSeq_sa_only <- spec.pgram(amb.doy$decomposition$sa, log = FALSE, plot = FALSE)
plot(periodogram_ambSeq$freq, periodogram_ambSeq$spec, type = "l", 
     col = "blue", lwd = 2,
     xlab = "Frequency", ylab = "Spectral density")
abline(v = c(1/7, 2/7, 3/7), col = "gray", lty = 2)              # Settimanale
abline(v = c(1/365.2425, 2/365.2425, 3/365.2425), col = "gray", lty = 4)  # Annuale
abline(v = freq_monthly, col = "darkred", lty = 3)  # Monthly
title(main = "Sequential AMB - SA series", font.main = 1)

periodogram_i_ambSeq <- spec.pgram(amb.doy$decomposition$i, log = FALSE, plot = FALSE)
plot(periodogram_i_ambSeq$freq, periodogram_i_ambSeq$spec, type = "l", 
     col = "blue", lwd = 2,
     xlab = "Frequency", ylab = "Spectral density")
abline(v = c(1/7, 2/7, 3/7), col = "gray", lty = 2)              # Settimanale
abline(v = c(1/365.2425, 2/365.2425, 3/365.2425), col = "gray", lty = 4)  # Annuale
abline(v = freq_monthly, col = "darkred", lty = 3)  # Monthly
title(main = "Multi AMB - irregular component", font.main = 1)


######################## irregular component analysis 

# AMB SEQUENTIAL approach
results_ambSeq_irr <- analyze_irregular(
  amb.doy$decomposition$i,
  name = "AMB Sequential"
)

################################ Residual calendar effects
library(rjd3toolkit)
library(lubridate)    
# global ftest
td_f(amb.doy$decomposition$sa, model = "D1", nyears = 0)


################################ Revisions
amb_estimator2 <- AMBEstimator$new(data = data, cal_reg_pre_pro = cal_reg)
results_amb2 <- calculate_revisions_by_lags(
  estimator_instance = amb_estimator2
)
results_amb2


################################################################################
######################### KDE analysis #########################################
################################################################################

ts_raw_data <- with(data[order(data$Date), ],
                    ts(Value,
                       start = c(as.numeric(format(min(Date), "%Y")),
                                 as.numeric(format(min(Date), "%j"))),
                       frequency = 365))

ts_sa_AMB_seq   <- ts(amb.doy$decomposition$sa,       start = c(as.numeric(format(dates[1], "%Y")), as.numeric(format(dates[1], "%j"))), frequency = 365)





xlim_range <- c(-0.5, 0.5) # adjust them basing on the plot ranges of these data
ylim_range <- c(0, 10)

KDE_AMB_seq   <- plot_kde_acf(raw_data = ts_raw_data, sa_data = ts_sa_AMB_seq  , lags = c(7, 365), window_cycles =  2, xlim_min = xlim_range[1], xlim_max = xlim_range[2], ylim_min = ylim_range[1], ylim_max = ylim_range[2], show_legend = TRUE, title_label = "KDE of sample ACF for AMB sequential")
KDE_AMB_seq

KDE_AMB_seq_month <- plot_kde_acf(raw_data = ts_raw_data, sa_data = ts_sa_AMB_seq  , lags = c(30), window_cycles =  2, xlim_min = xlim_range[1], xlim_max = xlim_range[2], ylim_min = ylim_range[1], ylim_max = ylim_range[2], show_legend = TRUE, title_label = "KDE of sample ACF for AMB sequential")
KDE_AMB_seq_month


################################################################################
######################### MONTHLY SA ###########################################
################################################################################
# AMB SEQUENTIAL


# aggregazione giornaliera -> mensile
res <- aggregate_daily_to_monthly(data, fun = mean)

# destagionalizzazione con i metodi tradizionali ad alta freq. (TRAMO-SEATS, X13, STS, STL, TBATS)
SA <- sa_all_methods(res)


SA = SA %>% 
  dplyr::filter(date >= as.Date("2020-01-01")) %>% 
  dplyr::filter(date <= as.Date("2020-12-31"))


data2      <- data
data2$Date <- as.Date(data2$Date)

data2 <- data2 %>% 
  dplyr::filter(Date >= as.Date("2020-01-01")) %>% 
  dplyr::filter(Date <= as.Date("2020-12-31"))

plot(data2$Date, data2$Value, type = "l",
     xlab = "Time", ylab = "", lwd = 1)
title(main = "Year 2020", font.main = 1)
plot(data2$Date, data2$Value, type = "l",
     xlab = "Time", ylab = "", lwd = 1)
# title(main = "Years 2012-2021", font.main = 1)

sa_amb <- data.frame(
  date = data$Date,
  sa = amb.doy$decomposition$sa
)


sa_amb <- sa_amb %>% 
  dplyr::filter(date >= as.Date("2020-01-01")) %>% 
  dplyr::filter(date <= as.Date("2020-12-31"))

sa_amb_monthly <- sa_amb %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(sa_monthly = mean(sa, na.rm = TRUE))


Sys.setlocale("LC_TIME", "C")   
layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))  
par(mar = c(5, 4, 4, 2))
plot(data2$Date, data2$Value, type = "l", col = "grey70",
     xlab = "Time", ylab = label_original, lwd = 1,
     xaxt = "n")
title(main = "Sequential AMB - Focus year 2020", font.main = 1)
axis.Date(1, at = seq(min(data2$Date), max(data2$Date), by = "month"),
          format = "%b")  
lines(sa_amb$date, sa_amb$sa, col = "orange", lwd = 2)
lines(sa_amb_monthly$month, sa_amb_monthly$sa_monthly,
      col = "red", lwd = 2, type = "b", pch = 17)
lines(SA$date, SA$TRAMO_SEATS, col = "purple", lwd = 2, type = "b", pch = 16)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = c("Raw (daily)", "SA daily", "SA daily → monthly mean", "SA monthly (TRAMO-SEATS)"),
       col = c("grey70", "orange", "red", "purple"),
       lty = c(1, 1, 1, 1), lwd = 2, pch = c(NA, NA, 17, 16),
       bty = "n", cex = 1.1)


################################################################################
#################                S-I Ratio                    ##################
################################################################################

out_amb_seq   <- compute_si_daily(log = FALSE, s7 = amb.dow$decomposition$s, i = amb.doy$decomposition$i, dates = data$Date)

par(blank_par)
si_amb_seq   <- plot_si_weekly(dates = out_amb_seq$daily$date, si = out_amb_seq$daily$si,  s7 = amb.dow$decomposition$s, log_ratio = FALSE, show = "points")


save.image(file="workspace_electronic payments.RData")

