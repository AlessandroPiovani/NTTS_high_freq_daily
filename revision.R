# adjust_x_pre_pro <- function(x_pre_pro, data_value) {
#   if (is.null(x_pre_pro)) {
#     return(NULL)
#   }
# 
#   n_pre <- length(x_pre_pro)
#   n_val <- length(data_value)
# 
#   if (n_pre > n_val) {
#     return(head(x_pre_pro, n_val))
#   } else {
#     return(x_pre_pro)
#   }
# }


adjust_x_pre_pro <- function(x_pre_pro, data_value) {
  if (is.null(x_pre_pro)) {
    return(NULL)
  }
  
  n_val <- length(data_value)
  
  if (is.vector(x_pre_pro)) {
    n_pre <- length(x_pre_pro)
    if (n_pre > n_val) {
      return(head(x_pre_pro, n_val))
    } else {
      return(x_pre_pro)
    }
  }
  
  if (is.matrix(x_pre_pro)) {
    n_pre <- nrow(x_pre_pro)
    if (n_pre > n_val) {
      return(x_pre_pro[seq_len(n_val), , drop = FALSE])
    } else {
      return(x_pre_pro)
    }
  }
  
  stop("x_pre_pro must be either a vector or a matrix")
}




pre_processing <- function(data, cal_reg = NA, dates_by = "day", dates_from = "2012-01-01", criticalValue = 6, periods = c(7, 365.2425), log = FALSE, outliers = c("ao","wo", "ls"), x_pre_pro = NULL)
{
  
  # browser()
  if(all(is.na(cal_reg)))
  {
    
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
    colnames(cal_reg) <- c("25th_apr", "2nd_jun", 
                           "8th_dec", "26th_dec","epiphany","1st_jan", "easter_mon", "1st_may", "15th_aug", 
                           "1st_nov", "xmas", "easter" )
  } 
  

  
  
  library(rjd3filters)
  sm2<-model()
  eq2<-equation("eq")
  add(sm2,sarima("airline",7,c(0,1,1),c(0,1,1)))
  # add(sm2,sarima("airline",365,c(0,1,1),c(0,1,1)))
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
  # colors <- c("#E41A1C", 
  #             "#377EB8", 
  #             "#4DAF4A", 
  #             "#984EA3", 
  #             "#FF7F00", 
  #             "#FFFF33", 
  #             "#A65628", 
  #             "#F781BF", 
  #             "#999999", 
  #             "#66C2A5", 
  #             "#FC8D62", 
  #             "#8DA0CB") 
  # 
  # y_range <- range(ss[,10:21], na.rm = TRUE)
  # options(scipen = 999)
  # layout(matrix(c(1,2), nrow = 2), heights = c(4, 1))
  # 
  # 
  # 
  # # grafico
  # plot(data$Date, ss[,10], type = "l", col = colors[1],
  #      xlab = "Time", ylab = if(log) "Calendar effect (log-scale)" else "Calendar effect",
  #      ylim = y_range, main = "")
  # 
  # for (i in 11:21) lines(data$Date, ss[,i], col = colors[i - 9])
  # 
  # mtext(paste0(label_original,": ","Calendar effects variation over time"), 
  #       side = 3, line = 1, font = 1, cex = 1.2, family = "sans")
  # 
  # # legenda sotto
  # par(mar = c(0, 0, 0, 2))
  # plot.new()
  # legend("center",
  #        legend = c("Liberation Day", "Republic Day", "Immaculate Conception", "St. Stephen's Day", "Epiphany",
  #                   "New Year’s Day", "Easter Monday", "Labour Day", "Assumption of Mary", "All Saints' Day", "Christmas", "Easter"),
  #        col = colors, lty = 1, ncol = 3, cex = 0.9, bty = "n")
  # layout(matrix(1))
  
  data$Time_Varying <- cmp[,2]
  
  if(log==TRUE)
  {
    data$Value_td_adjusted <- data$Value/exp(cmp[,2])
  }else{
    data$Value_td_adjusted <- data$Value - cmp[,2]    
  }
  
  
  # new
  
  # sm <- model()
  # eq <- equation("eq")
  # add(sm, sarima("airline", 7 , c(0,1,1),c(0,1,1)))
  # add(sm, reg("cal", cal_reg, 0.1))
  # 
  # if(log==TRUE)
  # {
  #   
  #   rslt <- rjd3sts::estimate(sm, log(data$Value), marginal = F, initialization = "SqrtDiffuse",
  #                             optimizer = "LevenbergMarquardt", 
  #                             concentrated = TRUE, precision = 1e-10,)
  #   # rslt_log <- estimate(sm, data$Log_Value, marginal = F, initialization = "SqrtDiffuse", optimizer = "LevenbergMarquardt", concentrated = TRUE, precision = 1e-10)
  #   ss   <- rjd3sts::smoothed_states(rslt)
  #   sc   <- rjd3sts::smoothed_components(rslt)
  #   # sc   <- rjd3sts::smoothed_components(rslt_log)
  #   dates <- seq.Date(from = as.Date(date_start), by = "day", length.out = nrow(sc))
  #   rownames(sc) <- as.character(dates)
  #   
  #   # browser()
  #   
  #   data$Time_Varying <- exp(sc[,2])
  #   data$Value_td_adjusted <- data$Value - exp(sc[,2])
  #   
  # } else{
  #   # browser()
  #   rslt <- rjd3sts::estimate(sm, data$Value, marginal = F, initialization = "SqrtDiffuse",
  #                             optimizer = "LevenbergMarquardt",
  #                             concentrated = TRUE, precision = 1e-10,)
  #   # rslt_log <- estimate(sm, data$Log_Value, marginal = F, initialization = "SqrtDiffuse", optimizer = "LevenbergMarquardt", concentrated = TRUE, precision = 1e-10)
  #   ss   <- rjd3sts::smoothed_states(rslt)
  #   sc   <- rjd3sts::smoothed_components(rslt)
  #   # sc   <- rjd3sts::smoothed_components(rslt_log)
  #   dates <- seq.Date(from = as.Date(date_start), by = "day", length.out = nrow(sc))
  #   rownames(sc) <- as.character(dates)
  #   
  #   data$Time_Varying <- sc[,2]
  #   data$Value_td_adjusted <- data$Value - sc[,2]
  #   
  # } 
  
  
  # if(log==TRUE)
  # {
  #   # OLD
  #   # rslt <- rjd3sts::estimate(sm, log(data$Value), marginal = F, initialization = "SqrtDiffuse",
  #   #                           optimizer = "LevenbergMarquardt", 
  #   #                           concentrated = TRUE, precision = 1e-10)
  #   # # rslt_log <- estimate(sm, data$Log_Value, marginal = F, initialization = "SqrtDiffuse", optimizer = "LevenbergMarquardt", concentrated = TRUE, precision = 1e-10)
  #   # ss   <- rjd3sts::smoothed_states(rslt)
  #   # sc   <- rjd3sts::smoothed_components(rslt)
  #   # # sc   <- rjd3sts::smoothed_components(rslt_log)
  #   # dates <- seq.Date(from = as.Date(date_start), by = "day", length.out = nrow(sc))
  #   # rownames(sc) <- as.character(dates)
  #   # 
  #   # # browser()
  #   # 
  #   # data$Time_Varying <- exp(sc[,2])
  #   # data$Value_td_adjusted <- data$Value - exp(sc[,2])
  #   
  #   # NEW
  #   rslt <- rjd3sts::estimate(sm, log(data$Value), marginal = F, initialization = "SqrtDiffuse",
  #                             optimizer = "LevenbergMarquardt",
  #                             concentrated = TRUE, precision = 1e-10,)
  #   java_object <- rslt$internal$getData("ssf.smoothing.states")
  #   raw_string <- java_object$toString()
  #   states_matrix <- as.matrix(read.table(text = raw_string, sep = "\t", header = FALSE))
  #   
  #   # 2. Extract the last column (corresponding to the calendar/regressor component)
  #   data$Time_Varying <- states_matrix[, ncol(states_matrix)]
  #   
  #   # 3. Date alignment and trading-day adjusted series calculation
  #   dates <- seq.Date(from = as.Date(date_start), by = "day", length.out = nrow(states_matrix))
  #   rownames(states_matrix) <- as.character(dates)
  #   
  #   data$Value_td_adjusted <- data$Value - exp(data$Time_Varying)
  #   
  #   
  # } else{
  #   # OLD
  #   
  #   # rslt <- rjd3sts::estimate(sm, data$Value, marginal = F, initialization = "SqrtDiffuse",
  #   #                           optimizer = "LevenbergMarquardt",
  #   #                           concentrated = TRUE, precision = 1e-10)
  #   # # rslt_log <- estimate(sm, data$Log_Value, marginal = F, initialization = "SqrtDiffuse", optimizer = "LevenbergMarquardt", concentrated = TRUE, precision = 1e-10)
  #   # ss   <- rjd3sts::smoothed_states(rslt)
  #   # sc   <- rjd3sts::smoothed_components(rslt)
  #   # # sc   <- rjd3sts::smoothed_components(rslt_log)
  #   # dates <- seq.Date(from = as.Date(date_start), by = "day", length.out = nrow(sc))
  #   # rownames(sc) <- as.character(dates)
  #   # 
  #   # data$Time_Varying <- sc[,2]
  #   # data$Value_td_adjusted <- data$Value - sc[,2]
  #   
  #   # 1. Parse the complete smoothing states matrix from Java
  #   
  #   # NEW
  #   rslt <- rjd3sts::estimate(sm, data$Value, marginal = F, initialization = "SqrtDiffuse",
  #                             optimizer = "LevenbergMarquardt",
  #                             concentrated = TRUE, precision = 1e-10)
  #   java_object <- rslt$internal$getData("ssf.smoothing.states")
  #   raw_string <- java_object$toString()
  #   states_matrix <- as.matrix(read.table(text = raw_string, sep = "\t", header = FALSE))
  #   
  #   # 2. Extract the last column (corresponding to the calendar/regressor component)
  #   data$Time_Varying <- states_matrix[, ncol(states_matrix)]
  #   
  #   # 3. Date alignment and trading-day adjusted series calculation
  #   dates <- seq.Date(from = as.Date(date_start), by = "day", length.out = nrow(states_matrix))
  #   rownames(states_matrix) <- as.character(dates)
  #   
  #   data$Value_td_adjusted <- data$Value - data$Time_Varying
  #   
  # } 
  
  
  
  # old
  # 
  # sm <- model()
  # eq <- equation("eq")
  # add(sm, sarima("airline", 7 , c(0,1,1),c(0,1,1)))
  # add(sm, reg("cal", cal_reg, 0.1))
  # rslt <- rjd3sts::estimate(sm, data$Value, marginal = F, initialization = "SqrtDiffuse",
  #                  optimizer = "LevenbergMarquardt",
  #                  concentrated = TRUE, precision = 1e-10)
  # sc   <- rjd3sts::smoothed_components(rslt)
  # 
  # dates                   <- seq.Date(from = as.Date(dates_from), by = dates_by, length.out = nrow(sc))
  # rownames(sc)            <- as.character(dates)
  # data$Time_Varying       <- sc[,2]
  # data$Value_td_adjusted <- data$Value - sc[,2]
  
  pre_pro <- fractionalAirlineEstimation(
    y             = data$Value_td_adjusted,
    x             = x_pre_pro,
    criticalValue = criticalValue ,
    periods       = periods, # weekly frequency
    outliers      = outliers,
    log           = log, y_time = data$Date)
  
  
  return(pre_pro)
}



################################################################################

calculate_mar_convergence_optimized <- function(estimator_instance, lag, delta_lag = floor(lag / 4)) {
  y <- estimator_instance$data$Value
  if (delta_lag <= 0) stop("delta_lag must be > 0")
  T <- length(y)
  # Esegui la stima sull'intero dataset
  estimator_full <- estimator_instance
  S_final <- estimator_full$run()
  if (length(S_final) != T) stop("run() must return vector of same length as y")
  results <- data.frame(
    lag_values = numeric(),
    MAR = numeric(),
    MAR_percent = numeric()
  )
  current_lag <- 1
  while (current_lag <= lag) {
    vintage_end <- T - current_lag
    y_vintage <- y[1:vintage_end]
    # Taglia l'intero data.frame, non solo Value
    vintage_data <- estimator_instance$data[1:vintage_end, ]
    # Crea una nuova istanza dell'estimatore con i dati ridotti
    estimator_vintage <- STLEstimator$new(data = vintage_data)
    S_vintage <- estimator_vintage$run()
    
    
    # Confronta fin dove ci sono i dati
    # common_t <- 1:(vintage_end - 1)
    common_t <- 1:(vintage_end) # ancora meglio senza il -1
    
    
    # Confronta solo dove ci sono dati comuni
    # common_t <- 1:(T-lag)
    
    r_t <- S_final[common_t] - S_vintage[common_t]
    mar <- mean(abs(r_t), na.rm = TRUE)
    mar_percent <- mean(abs(r_t / S_final[common_t]), na.rm = TRUE) * 100
    results <- rbind(results, data.frame(
      lag_values = current_lag,
      MAR = mar,
      MAR_percent = mar_percent
    ))
    current_lag <- current_lag + delta_lag
  }
  
  # Calcolo delle medie finali
  results$MAR_mean <- mean(results$MAR, na.rm = TRUE)
  results$MAR_percent_mean <- mean(results$MAR_percent, na.rm = TRUE)
  # Plot con ggplot2
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    plot <- ggplot(results, aes(x = lag_values)) +
      geom_line(aes(y = MAR_percent, color = "MAR (%)")) +
      labs(title = "MAR (%) over Increasing Lags",
           x = "Lag (days)",
           y = "Revision Magnitude",
           color = "Metric") +
      theme_minimal()
    print(plot)
  } else {
    warning("ggplot2 not installed. Plot not shown.")
  }
  return(results)
}





calculate_revisions_by_lags <- function(estimator_instance, 
                                        lags = c(7, 30, 90, 181, 365, 730)) {
  
  # browser()
  
  y <- estimator_instance$data$Value
  T <- length(y)
  
  # Stima full sample
  estimator_full <- estimator_instance
  S_final <- estimator_full$run()
  if (length(S_final) != T) stop("run() must return vector of same length as y")
  
  cat("\n=== Model variables — full sample ===\n")
  print(estimator_full$last_pre_pro$model$variables)
  
  variables_by_lag <- list()
  variables_by_lag[["full_sample"]] <- estimator_full$last_pre_pro$model$variables
  
  
  results <- data.frame(
    lag_values = numeric(),
    MR = numeric(),
    MAR = numeric(),
    RMAR = numeric(),
    SD = numeric(),
    RMSR = numeric(),
    MinRev = numeric(),
    MaxRev = numeric()
  )
  
  for (current_lag in lags) {
    if (current_lag >= T) next
    vintage_end <- T - current_lag
    # browser()
    vintage_data <- estimator_instance$data[1:vintage_end, ]
    
    # Nuova istanza vintage come STLEstimator
    #estimator_vintage <- STLEstimator$new(data = vintage_data)
    
    # Nuova istanza della *stessa* classe di estimator_instance
    # cls_name <- class(estimator_instance)[1]
    # estimator_vintage <- get(cls_name)$new(data = vintage_data)
    # Modalità che non da errore
    
    # Old
    # estimator_vintage <- estimator_instance$clone(deep = TRUE)
    # estimator_vintage$data <- vintage_data
    
    # New
    estimator_vintage <- estimator_instance$clone(deep = TRUE)   # <-- questa riga manca!
    estimator_vintage$data <- vintage_data
    if (!is.null(estimator_vintage$cal_reg_pre_pro) && !identical(estimator_vintage$cal_reg_pre_pro, NA)) {
      estimator_vintage$cal_reg_pre_pro <- estimator_vintage$cal_reg_pre_pro[1:vintage_end, , drop = FALSE]
    }
    if (!is.null(estimator_vintage$x_pre_pro)) {
      estimator_vintage$x_pre_pro <- estimator_vintage$x_pre_pro[1:vintage_end, , drop = FALSE]  # o vettore, dipende dal formato
    }
    
    
    
    
    S_vintage <- estimator_vintage$run()
    
    cat("\n=== Model variables — lag =", current_lag, "===\n")
    print(estimator_vintage$last_pre_pro$model$variables)
    
    variables_by_lag[[paste0("lag_", current_lag)]] <- estimator_vintage$last_pre_pro$model$variables
    
    # Confronto sui dati comuni (campione costante per robustezza)
    common_t <- 1:(T - max(lags))
    
    # Alternativa: arrivare fino alla fine del vintage
    # common_t <- 1:(vintage_end - 1)
    
    r_t <- S_final[common_t] - S_vintage[common_t]
    
    # Diagnostiche
    mr   <- mean(r_t, na.rm = TRUE)
    mar  <- mean(abs(r_t), na.rm = TRUE)
    rmar <- mean(abs(r_t / S_final[common_t]), na.rm = TRUE) * 100 # Old
    
    # Se denominatore piccolo, al suo posto uso 1%*dev_standard che è più alto (così RMAR non esplode)
    # scale_ref <- sd(S_final[common_t], na.rm = TRUE)
    # denom     <- pmax(abs(S_final[common_t]), 0.01 * scale_ref)
    # rmar      <- mean(abs(r_t) / denom, na.rm = TRUE) * 100 # new
    
    sd_r <- sd(r_t, na.rm = TRUE)
    rmsr <- sqrt(mean(r_t^2, na.rm = TRUE))
    min_r <- min(r_t, na.rm = TRUE)
    max_r <- max(r_t, na.rm = TRUE)
    
    results <- rbind(results, data.frame(
      lag_values = current_lag,
      MR = mr,
      MAR = mar,
      RMAR = rmar,
      SD = sd_r,
      RMSR = rmsr,
      MinRev = min_r,
      MaxRev = max_r
    ))
  }
  
  # Medie finali (su tutti i lag considerati)
  results$MR_mean    <- mean(results$MR, na.rm = TRUE)
  results$MAR_mean   <- mean(results$MAR, na.rm = TRUE)
  results$RMAR_mean  <- mean(results$RMAR, na.rm = TRUE)
  results$SD_mean    <- mean(results$SD, na.rm = TRUE)
  results$RMSR_mean  <- mean(results$RMSR, na.rm = TRUE)
  
  # Plot (solo RMAR per leggibilità)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    plot <- ggplot(results, aes(x = lag_values)) +
      geom_line(aes(y = RMAR, color = "RMAR (%)")) +
      labs(title = "Revision Diagnostics over Selected Lags",
           x = "Lag (days)",
           y = "Revision Magnitude",
           color = "Metric") +
      theme_minimal()
    print(plot)
  }
  
  return(results)
}



################################################################################
library(R6)

# CLasse generica che obbliga ad avere il metodo Run
SignalEstimator <- R6Class("SignalEstimator",
                           public = list(
                             data = NULL,
                             initialize = function(data) {
                               self$data <- data
                             },
                             run = function() {
                               stop("run() must be implemented in a subclass")
                             }
                           )
)


################################################################################
######################## STL
# Specializzazione STL estimator, tailored su STL: dentro initialize mettere tutti gli argomenti che servono

STLSEQEstimator <- R6::R6Class("STLSEQEstimator",
                               inherit = SignalEstimator,
                               public = list(
                                 data = NULL,
                                 x_pre_pro = NULL,
                                 cal_reg_pre_pro = NULL,
                                 dates_by_pre_pro = NULL,
                                 dates_from_pre_pro = NULL,
                                 criticalValue_pre_pro = NULL,
                                 periods_pre_pro = NULL,
                                 log_pre_pro = NULL,
                                 outliers_pre_pro = NULL,
                                 period_s1 = NULL,
                                 multiplicative_s1 = NULL,
                                 swindow_s1 = NULL,
                                 twindow_s1 = NULL,
                                 sdegree_s1 = NULL,
                                 tdegree_s1 = NULL,
                                 ninnerloop_s1 = NULL,
                                 nouterloop_s1 = NULL,
                                 period_s2 = NULL,
                                 multiplicative_s2 = NULL,
                                 swindow_s2 = NULL,
                                 twindow_s2 = NULL,
                                 sdegree_s2 = NULL,
                                 tdegree_s2 = NULL,
                                 ninnerloop_s2 = NULL,
                                 nouterloop_s2 = NULL,
                                 last_pre_pro = NULL,   # <-- AGGIUNGI QUESTA RIGA
                                 
                                 initialize = function(data,
                                                       # ==== Parametri pre_processing ====
                                                       x_pre_pro            = NULL,
                                                       cal_reg_pre_pro      = NA,
                                                       dates_by_pre_pro     = "day",
                                                       dates_from_pre_pro   = "2012-01-01",
                                                       criticalValue_pre_pro = 6,
                                                       periods_pre_pro      = c(7, 365.2425),
                                                       log_pre_pro          = FALSE,
                                                       outliers_pre_pro     = c("ao","wo", "ls"),
                                                       # ==== Parametri stlplus: s1 ====
                                                       period_s1      = 7,
                                                       multiplicative_s1 = FALSE,
                                                       swindow_s1     = 11,
                                                       twindow_s1     = 19,
                                                       sdegree_s1     = 0,
                                                       tdegree_s1     = 1,
                                                       ninnerloop_s1  = 2,
                                                       nouterloop_s1  = 0,
                                                       # ==== Parametri stlplus: s2 ====
                                                       period_s2      = 365.2425,
                                                       multiplicative_s2 = FALSE,
                                                       swindow_s2     = 7,
                                                       twindow_s2     = 735,
                                                       sdegree_s2     = 0,
                                                       tdegree_s2     = 1,
                                                       ninnerloop_s2  = 2,
                                                       nouterloop_s2  = 0) {
                                   super$initialize(data)
                                   
                                   # salvo tutti i parametri nell'oggetto
                                   self$data <- data
                                   self$x_pre_pro            <- x_pre_pro
                                   self$cal_reg_pre_pro      <- cal_reg_pre_pro
                                   self$dates_by_pre_pro     <- dates_by_pre_pro
                                   self$dates_from_pre_pro   <- dates_from_pre_pro
                                   self$criticalValue_pre_pro <- criticalValue_pre_pro
                                   self$periods_pre_pro      <- periods_pre_pro
                                   self$log_pre_pro          <- log_pre_pro
                                   self$outliers_pre_pro     <- outliers_pre_pro
                                   self$period_s1       <- period_s1
                                   self$multiplicative_s1 <- multiplicative_s1
                                   self$swindow_s1      <- swindow_s1
                                   self$twindow_s1      <- twindow_s1
                                   self$sdegree_s1      <- sdegree_s1
                                   self$tdegree_s1      <- tdegree_s1
                                   self$ninnerloop_s1   <- ninnerloop_s1
                                   self$nouterloop_s1   <- nouterloop_s1
                                   self$period_s2       <- period_s2
                                   self$multiplicative_s2 <- multiplicative_s2
                                   self$swindow_s2      <- swindow_s2
                                   self$twindow_s2      <- twindow_s2
                                   self$sdegree_s2      <- sdegree_s2
                                   self$tdegree_s2      <- tdegree_s2
                                   self$ninnerloop_s2   <- ninnerloop_s2
                                   self$nouterloop_s2   <- nouterloop_s2
                                 },
                                 
                                 run = function() {  
                                   
                                   # browser()
                                   
                                   require(rjd3sts)
                                   require(rjd3toolkit)
                                   require(rjd3highfreq)
                                   require(rjd3stl)
                                   
                                   
                                   pre_pro <- pre_processing(
                                     data          = self$data,
                                     x_pre_pro     = adjust_x_pre_pro(self$x_pre_pro, self$data$Value),
                                     cal_reg       = self$cal_reg_pre_pro,
                                     dates_by      = self$dates_by_pre_pro,
                                     dates_from    = self$dates_from_pre_pro,
                                     criticalValue = self$criticalValue_pre_pro,
                                     periods       = self$periods_pre_pro,
                                     log           = self$log_pre_pro,
                                     outliers      = self$outliers_pre_pro
                                   )
                                   
                                   self$last_pre_pro <- pre_pro
                                   
                                   is_ls_present <- !is.null(pre_pro$model$component_ls) && 
                                     length(pre_pro$model$component_ls) > 0 && 
                                     !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls))
                                   
                                   # prima stlplus (s1)
                                   s1_stl <- stlplus(
                                     pre_pro$model$linearized,
                                     period = self$period_s1,
                                     multiplicative = self$multiplicative_s1,
                                     swindow = self$swindow_s1,     
                                     twindow = self$twindow_s1,     
                                     sdegree = self$sdegree_s1,
                                     tdegree = self$tdegree_s1,
                                     ninnerloop = self$ninnerloop_s1,
                                     nouterloop = self$nouterloop_s1
                                   )
                                   
                                   # seconda stlplus (s2)
                                   s2_stl <- stlplus(
                                     s1_stl$decomposition[,'sa'],
                                     period = self$period_s2,
                                     multiplicative = self$multiplicative_s2,
                                     swindow = self$swindow_s2,      
                                     twindow = self$twindow_s2,    
                                     sdegree = self$sdegree_s2,
                                     tdegree = self$tdegree_s2,
                                     ninnerloop = self$ninnerloop_s2,
                                     nouterloop = self$nouterloop_s2
                                   )
                                   
                                   
                                   if(is_ls_present) {

                                       s2_stl$decomposition[,'sa'] <- s2_stl$decomposition[,'sa'] + pre_pro$model$component_ls
                                     
                                   }
                                   
                                   if(self$log_pre_pro)
                                   {
                                     s2_stl$decomposition[,'sa'] <- exp(s2_stl$decomposition[,'sa'])
                                   }   
                                   
                                   s2_stl$decomposition[,'sa'] # serie SA
                                 }
                               )
)

# stl_estimator <- STLSEQEstimator$new(data = data)
# results_stl <- calculate_mar_convergence_optimized(
#   estimator_instance = stl_estimator,
#   lag = 365
# )
# results_stl
# 
# 
# stl_estimator2 <- STLSEQEstimator$new(data = data)
# results_stl2 <- calculate_revisions_by_lags(
#   estimator_instance = stl_estimator2
# )
# results_stl2
# # STL: MAR_percent_mean=0.15

############################# STL ITERATIVE
STLEstimatorIt <- R6::R6Class("STLEstimatorIt",
                              inherit = SignalEstimator,
                              public = list(
                                data = NULL,
                                x_pre_pro = NULL,
                                cal_reg = NULL,
                                dates_by = NULL,
                                dates_from = NULL,
                                criticalValue = NULL,
                                periods_pre = NULL,
                                log = NULL,
                                outliers = NULL,
                                periods = NULL,
                                swindows = NULL,
                                twindow = NULL,
                                multiplicative = NULL,
                                ninnerloop = NULL,
                                nouterloop = NULL,
                                last_pre_pro = NULL,   # <-- AGGIUNGI QUESTA RIGA
                                
                                initialize = function(data,
                                                      x_pre_pro = NULL,
                                                      cal_reg = NA,
                                                      dates_by = "day",
                                                      dates_from = "2012-01-01",
                                                      criticalValue = 6,
                                                      periods_pre = c(7, 365.2425),
                                                      log = FALSE,
                                                      outliers = c("ao","wo", "ls"),
                                                      # Parametri STL singolo periodo
                                                      periods = c(7,365.2425),
                                                      swindows = c(11, 7),
                                                      twindow  = c(19, 735),
                                                      multiplicative = FALSE,
                                                      ninnerloop = 2,
                                                      nouterloop = 0) {
                                  
                                  
                                  self$data <- data
                                  self$x_pre_pro <- x_pre_pro
                                  
                                  # Salvo tutti i parametri come campi dell'oggetto
                                  self$cal_reg <- cal_reg
                                  self$dates_by <- dates_by
                                  self$dates_from <- dates_from
                                  self$criticalValue <- criticalValue
                                  self$periods_pre <- periods_pre
                                  self$log <- log
                                  self$outliers <- outliers
                                  self$periods <- periods
                                  self$swindows <- swindows
                                  self$twindow <- twindow
                                  self$multiplicative <- multiplicative
                                  self$ninnerloop <- ninnerloop
                                  self$nouterloop <- nouterloop
                                },
                                
                                run = function() {
                                  require(rjd3sts)
                                  require(rjd3toolkit)
                                  require(rjd3highfreq)
                                  require(rjd3stl)
                                  
                                  # browser()
                                  
                                  data <- self$data
                                  pre_pro <- pre_processing(
                                    data,
                                    x_pre_pro = adjust_x_pre_pro(self$x_pre_pro, self$data$Value),
                                    cal_reg = self$cal_reg,
                                    dates_by = self$dates_by,
                                    dates_from = self$dates_from,
                                    criticalValue = self$criticalValue,
                                    periods = self$periods_pre,
                                    log = self$log,
                                    outliers = self$outliers 
                                  )
                                  
                                  self$last_pre_pro <- pre_pro           # <-- Passo 3
                                  
                                  is_ls_present <- !is.null(pre_pro$model$component_ls) && 
                                    length(pre_pro$model$component_ls) > 0 && 
                                    !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls))
                                  
                                  stl_result <- istl(
                                    pre_pro$model$linearized,
                                    periods = self$periods,
                                    multiplicative = self$multiplicative, # do not touch the default = FALSE, because log is already done in the preprocessing
                                    swindow = self$swindows,
                                    twindow = self$twindow,
                                    ninnerloop = self$ninnerloop,
                                    nouterloop = self$nouterloop
                                  )
                                  
                                  # ---- re-add LSs ----
                                  
                                  if(is_ls_present) {

                                    stl_result$decomposition[,"sa"] <- stl_result$decomposition[,"sa"] + pre_pro$model$component_ls

                                  }
                                  
                                  # ---
                                  
                                  if(self$log)
                                  {
                                    stl_result$decomposition[,"sa"] <- exp(stl_result$decomposition[,"sa"])
                                  }   
                                  
                                  return(stl_result$decomposition[,"sa"])
                                }
                              )
)


# stl_estimator_multi <- STLEstimatorMulti$new(data = data)
# results_stlmulti <- calculate_mar_convergence_optimized(
#   estimator_instance = stl_estimator_multi,
#   lag = 365
# )
# results_stlmulti
# 
# stl_estimator_multi2 <- STLEstimatorMulti$new(data = data)
# results_stlmulti2 <- calculate_revisions_by_lags(
#   estimator_instance = stl_estimator_multi2
# )
# results_stlmulti2
# # STL: MAR_percent_mean=0.15
# # STL_multi: MAR_percent_mean=2.40





############################# STL MULTI

STLEstimatorMulti <- R6::R6Class("STLEstimatorMulti",
                                 inherit = SignalEstimator,
                                 public = list(
                                   data = NULL,
                                   x_pre_pro = NULL,
                                   cal_reg = NULL,
                                   dates_by = NULL,
                                   dates_from = NULL,
                                   criticalValue = NULL,
                                   periods_pre = NULL,
                                   log = NULL,
                                   outliers = NULL,
                                   periods = NULL,
                                   swindows = NULL,
                                   twindow = NULL,
                                   multiplicative = NULL,
                                   ninnerloop = NULL,
                                   nouterloop = NULL,
                                   last_pre_pro = NULL,   # <-- AGGIUNGI QUESTA RIGA
                                   
                                   initialize = function(data,
                                                         x_pre_pro = NULL,
                                                         cal_reg = NA,
                                                         dates_by = "day",
                                                         dates_from = "2012-01-01",
                                                         criticalValue = 6,
                                                         periods_pre = c(7, 365.2425),
                                                         log = FALSE,
                                                         outliers = c("ao","wo", "ls"),
                                                         # Parametri STL singolo periodo
                                                         periods = c(7,365),
                                                         swindows = c(11, 7),
                                                         twindow  = 735,
                                                         multiplicative = FALSE,
                                                         ninnerloop = 2,
                                                         nouterloop = 1) {
                                     
                                     self$data <- data
                                     self$x_pre_pro <- x_pre_pro
                                     
                                     # Salvo tutti i parametri come campi dell'oggetto
                                     self$cal_reg <- cal_reg
                                     self$dates_by <- dates_by
                                     self$dates_from <- dates_from
                                     self$criticalValue <- criticalValue
                                     self$periods_pre <- periods_pre
                                     self$log <- log
                                     self$outliers <- outliers
                                     self$periods <- periods
                                     self$swindows <- swindows
                                     self$twindow <- twindow
                                     self$multiplicative <- multiplicative
                                     self$ninnerloop <- ninnerloop
                                     self$nouterloop <- nouterloop
                                   },
                                   
                                   run = function() {
                                     require(rjd3sts)
                                     require(rjd3toolkit)
                                     require(rjd3highfreq)
                                     require(rjd3stl)
                                     
                                     # browser()
                                     
                                     data <- self$data
                                     pre_pro <- pre_processing(
                                       data,
                                       x_pre_pro = adjust_x_pre_pro(self$x_pre_pro, self$data$Value),
                                       cal_reg = self$cal_reg,
                                       dates_by = self$dates_by,
                                       dates_from = self$dates_from,
                                       criticalValue = self$criticalValue,
                                       periods = self$periods_pre,
                                       log = self$log,
                                       outliers = self$outliers
                                     )
                                     
                                     self$last_pre_pro <- pre_pro
                                     
                                     is_ls_present <- !is.null(pre_pro$model$component_ls) && 
                                       length(pre_pro$model$component_ls) > 0 && 
                                       !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls))
                                     
                                     stl_result <- rjd3stl::mstl(
                                       pre_pro$model$linearized,
                                       periods = self$periods,
                                       multiplicative = self$multiplicative,
                                       swindows = self$swindows,
                                       twindow  = self$twindow,      # trend globale singolo
                                       ninnerloop = self$ninnerloop,      # default MSTL
                                       nouterloop = self$nouterloop      #  0 because I use the pre-processed series (no need to robustify for outliers)
                                     )                                   # 0 is not accepted by the function, I need to put at least 1
                                     
                                     # ---- re-add LSs ----
                                     
                                     if(is_ls_present) {

                                         stl_result$decomposition[,"sa"] <- stl_result$decomposition[,"sa"] + pre_pro$model$component_ls
                                       
                                     }
                                     
                                     # ---
                                     
                                     if(self$log)
                                     {
                                       stl_result$decomposition[,"sa"] <- exp(stl_result$decomposition[,"sa"])
                                     }   
                                     
                                     return(stl_result$decomposition[,"sa"])
                                   }
                                 )
)


# stl_estimator_multi <- STLEstimatorMulti$new(data = data)
# results_stlmulti <- calculate_mar_convergence_optimized(
#   estimator_instance = stl_estimator_multi,
#   lag = 365
# )
# results_stlmulti
# 
# stl_estimator_multi2 <- STLEstimatorMulti$new(data = data)
# results_stlmulti2 <- calculate_revisions_by_lags(
#   estimator_instance = stl_estimator_multi2
# )
# results_stlmulti2
# # STL: MAR_percent_mean=0.15
# # STL_multi: MAR_percent_mean=2.40




############################ X11
X11Estimator <- R6::R6Class("X11Estimator",
                            inherit = SignalEstimator,
                            public = list(
                              
                              # ==== Dichiarazione campi ====
                              data = NULL,
                              x_pre_pro = NULL,
                              cal_reg_pre_pro = NULL,
                              dates_by_pre_pro = NULL,
                              dates_from_pre_pro = NULL,
                              criticalValue_pre_pro = NULL,
                              periods_pre_pro = NULL,
                              log_pre_pro = NULL,
                              outliers_pre_pro = NULL,
                              
                              period_s1 = NULL,
                              multiplicative_s1 = NULL,
                              trend_horizon_s1 = NULL,
                              trend_degree_s1 = NULL,
                              trend_kernel_s1 = NULL,
                              seas_s0_s1 = NULL,
                              seas_s1_s1 = NULL,
                              
                              period_s2 = NULL,
                              multiplicative_s2 = NULL,
                              trend_horizon_s2 = NULL,
                              trend_degree_s2 = NULL,
                              trend_kernel_s2 = NULL,
                              seas_s0_s2 = NULL,
                              seas_s1_s2 = NULL,
                              last_pre_pro = NULL,   # <-- AGGIUNGI QUESTA RIGA
                              
                              initialize = function(data,
                                                    # ==== Parametri pre_processing ====
                                                    cal_reg_pre_pro      = NA,
                                                    x_pre_pro = NULL,
                                                    dates_by_pre_pro     = "day",
                                                    dates_from_pre_pro   = "2012-01-01",
                                                    criticalValue_pre_pro = 6,
                                                    periods_pre_pro      = c(7,365.2425),
                                                    log_pre_pro          = FALSE,
                                                    outliers_pre_pro     = c("ao","wo", "ls"),
                                                    # ==== Parametri X11 s1 ====
                                                    period_s1        = 7,
                                                    multiplicative_s1 = FALSE,
                                                    trend_horizon_s1  = 9,
                                                    trend_degree_s1   = 3,
                                                    trend_kernel_s1   = "Henderson",
                                                    seas_s0_s1        = "S3X9",
                                                    seas_s1_s1        = "S3X9",
                                                    # ==== Parametri X11 s2 ====
                                                    period_s2        = 365.2425,
                                                    multiplicative_s2 = FALSE,
                                                    trend_horizon_s2  = 367,
                                                    trend_degree_s2   = 3,
                                                    trend_kernel_s2   = "Henderson",
                                                    seas_s0_s2        = "S3X5",
                                                    seas_s1_s2        = "S3X5") {
                                
                                super$initialize(data)
                                
                                # salvo tutti i parametri nell'oggetto
                                self$data <- data
                                self$x_pre_pro <- x_pre_pro
                                self$cal_reg_pre_pro      <- cal_reg_pre_pro
                                self$dates_by_pre_pro     <- dates_by_pre_pro
                                self$dates_from_pre_pro   <- dates_from_pre_pro
                                self$criticalValue_pre_pro <- criticalValue_pre_pro
                                self$periods_pre_pro      <- periods_pre_pro
                                self$log_pre_pro          <- log_pre_pro
                                self$outliers_pre_pro     <- outliers_pre_pro
                                self$period_s1       <- period_s1
                                self$multiplicative_s1 <- multiplicative_s1
                                self$trend_horizon_s1 <- trend_horizon_s1
                                self$trend_degree_s1  <- trend_degree_s1
                                self$trend_kernel_s1  <- trend_kernel_s1
                                self$seas_s0_s1       <- seas_s0_s1
                                self$seas_s1_s1       <- seas_s1_s1
                                self$period_s2       <- period_s2
                                self$multiplicative_s2 <- multiplicative_s2
                                self$trend_horizon_s2 <- trend_horizon_s2
                                self$trend_degree_s2  <- trend_degree_s2
                                self$trend_kernel_s2  <- trend_kernel_s2
                                self$seas_s0_s2       <- seas_s0_s2
                                self$seas_s1_s2       <- seas_s1_s2
                              },
                              
                              run = function() {
                                require(rjd3sts)
                                require(rjd3toolkit)
                                require(rjd3highfreq)
                                require(rjd3x11plus)
                                
                                # browser()
                                
                                # ---- pre_processing parametrico
                                pre_pro <- pre_processing(
                                  data          = self$data,
                                  x_pre_pro     = adjust_x_pre_pro(self$x_pre_pro, self$data$Value),
                                  cal_reg       = self$cal_reg_pre_pro,
                                  dates_by      = self$dates_by_pre_pro,
                                  dates_from    = self$dates_from_pre_pro,
                                  criticalValue = self$criticalValue_pre_pro,
                                  periods       = self$periods_pre_pro,
                                  log           = self$log_pre_pro,
                                  outliers      = self$outliers_pre_pro
                                )
                                
                                self$last_pre_pro <- pre_pro
                                
                                is_ls_present <- !is.null(pre_pro$model$component_ls) && 
                                  length(pre_pro$model$component_ls) > 0 && 
                                  !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls))
                                
                                # ---- prima X11 (s1)
                                s1_x11 <- x11plus(
                                  pre_pro$model$linearized,
                                  period = self$period_s1,
                                  mul = self$multiplicative_s1,
                                  trend.horizon = self$trend_horizon_s1,
                                  trend.degree  = self$trend_degree_s1,
                                  trend.kernel  = self$trend_kernel_s1,
                                  seas.s0       = self$seas_s0_s1,
                                  seas.s1       = self$seas_s1_s1
                                )
                                
                                # ---- seconda X11 (s2)
                                s2_x11 <- x11plus(
                                  s1_x11$decomposition$sa,
                                  period = self$period_s2,
                                  mul = self$multiplicative_s2,
                                  trend.horizon = self$trend_horizon_s2,
                                  trend.degree  = self$trend_degree_s2,
                                  trend.kernel  = self$trend_kernel_s2,
                                  seas.s0       = self$seas_s0_s2,
                                  seas.s1       = self$seas_s1_s2
                                )
                                
                                # --- re-add LS ---
                                if (is_ls_present) {
                                  if (self$log_pre_pro) {
                                    s2_x11$decomposition$sa <- s2_x11$decomposition$sa + log(pre_pro$model$component_ls)
                                  } else {
                                    s2_x11$decomposition$sa <- s2_x11$decomposition$sa + pre_pro$model$component_ls
                                  }
                                }
                                # ---
                                
                                if(self$log_pre_pro)
                                {
                                  s2_x11$decomposition$sa <- exp(s2_x11$decomposition$sa)
                                }  
                                
                                s2_x11$decomposition$sa
                              }
                            )
)


# x11_estimator <- X11Estimator$new(data = data)
# results_x11 <- calculate_mar_convergence_optimized(
#   estimator_instance = x11_estimator,
#   lag = 365
# )
# results_x11
# 
# # Qualche errore nei parametri? al limite usare la percentuale de x11:estimator
# # x11_estimator2 <- X11Estimator$new(data = data)
# # results_x112 <- calculate_revisions_by_lags(
# #   estimator_instance = x11_estimator2
# # )
# # results_x112
# 
# # STL: MAR_percent_mean=0.15
# # STL_multi: MAR_percent_mean=2.40
# # X11: MAR_percent_mean=1.28



############################ STS
STSEstimator <- R6::R6Class("STSEstimator",
                            inherit = SignalEstimator,
                            public = list(
                              data = NULL,
                              x_pre_pro = NULL,
                              cal_reg_pre_pro = NULL,
                              dates_by_pre_pro = NULL,
                              dates_from_pre_pro = NULL,
                              criticalValue_pre_pro = NULL,
                              periods_pre_pro = NULL,
                              log_pre_pro = NULL,
                              outliers_pre_pro = NULL,
                              levelVariance = NULL,
                              seasonal1_period = NULL,
                              seasonal1_variance = NULL,
                              seasonal2_period = NULL,
                              seasonal2_variance = NULL,
                              noise_variance = NULL,
                              optimizer = NULL,
                              precision = NULL,
                              locallineartrend = NULL, # to control the addition (or not addition) of a local linear trend: TRUE= add, FALSE= NOT ADD
                              localleveltrend  = NULL, # same for localleveltrend
                              use_splines = NULL,
                              n_knots_splines = NULL,
                              last_pre_pro = NULL,   # <-- AGGIUNGI QUESTA RIGA
                              
                              initialize = function(data,
                                                    # ==== Parametri pre_processing ====
                                                    cal_reg_pre_pro      = NA,
                                                    x_pre_pro = NULL,
                                                    dates_by_pre_pro     = "day",
                                                    dates_from_pre_pro   = "2012-01-01",
                                                    criticalValue_pre_pro = 6,
                                                    periods_pre_pro      = c(7, 365.2425),
                                                    log_pre_pro          = FALSE,
                                                    outliers_pre_pro     = c("ao","wo", "ls"),
                                                    # ==== Parametri STS ====
                                                    levelVariance       = 0.01,
                                                    seasonal1_period    = 7,
                                                    seasonal1_variance  = 0.01,
                                                    seasonal2_period    = 365.2425,
                                                    seasonal2_variance  = 0.01,
                                                    noise_variance      = 0.01,
                                                    optimizer           = "LevenbergMarquardt",
                                                    precision           = 1e-10,
                                                    locallineartrend    = TRUE,
                                                    localleveltrend     = FALSE,
                                                    use_splines         = FALSE,
                                                    n_knots_splines     = 60) { 
                                
                                super$initialize(data)
                                
                                # salvo tutti i parametri nell'oggetto
                                self$data <- data
                                self$x_pre_pro            <- x_pre_pro
                                self$cal_reg_pre_pro      <- cal_reg_pre_pro
                                self$dates_by_pre_pro     <- dates_by_pre_pro
                                self$dates_from_pre_pro   <- dates_from_pre_pro
                                self$criticalValue_pre_pro <- criticalValue_pre_pro
                                self$periods_pre_pro      <- periods_pre_pro
                                self$log_pre_pro          <- log_pre_pro
                                self$outliers_pre_pro     <- outliers_pre_pro
                                self$levelVariance        <- levelVariance
                                self$seasonal1_period     <- seasonal1_period
                                self$seasonal1_variance   <- seasonal1_variance
                                self$seasonal2_period     <- seasonal2_period
                                self$seasonal2_variance   <- seasonal2_variance
                                self$noise_variance       <- noise_variance
                                self$optimizer            <- optimizer
                                self$precision            <- precision
                                self$locallineartrend     <- locallineartrend
                                self$localleveltrend      <- localleveltrend
                                self$use_splines          <- use_splines 
                                self$n_knots_splines      <- n_knots_splines
                              },
                              
                              run = function() {
                                require(rjd3sts)
                                require(rjd3toolkit)
                                require(rjd3highfreq)
                                
                                # ---- pre_processing parametrico
                                pre_pro <- pre_processing(
                                  data          = self$data,
                                  x_pre_pro     = adjust_x_pre_pro(self$x_pre_pro, self$data$Value),
                                  cal_reg       = self$cal_reg_pre_pro,
                                  dates_by      = self$dates_by_pre_pro,
                                  dates_from    = self$dates_from_pre_pro,
                                  criticalValue = self$criticalValue_pre_pro,
                                  periods       = self$periods_pre_pro,
                                  log           = self$log_pre_pro,
                                  outliers      = self$outliers_pre_pro
                                )
                                
                                self$last_pre_pro <- pre_pro
                                
                                is_ls_present <- !is.null(pre_pro$model$component_ls) && 
                                  length(pre_pro$model$component_ls) > 0 && 
                                  !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls))
                                
                                # ---- modello STS parametrico
                                model_sts <- rjd3sts::model()
                                if(self$locallineartrend==TRUE)
                                {
                                  rjd3sts::add(model_sts, rjd3sts::locallineartrend("ll", levelVariance = self$levelVariance))
                                }
                                else if(self$localleveltrend==TRUE) # se c'è un locallineartrend non mette il locallevel
                                {
                                  rjd3sts::add(model_sts, rjd3sts::locallevel("l", variance = self$levelVariance)) #add local level trend
                                }  
                                rjd3sts::add(model_sts, rjd3sts::seasonal("s7", self$seasonal1_period, variance = self$seasonal1_variance, fixed = FALSE))
                                
                                if(!self$use_splines)
                                {  
                                  rjd3sts::add(model_sts, rjd3sts::seasonal("s365", self$seasonal2_period, variance = self$seasonal2_variance, fixed = FALSE))
                                }else
                                {
                                  # browser()
                                  seas365splines<-splines_regular("s365", 365, nknots = self$n_knots_splines, knots=c(0:self$n_knots_splines -1), variance = self$seasonal2_variance, fixed = FALSE)
                                  rjd3sts::add(model_sts, seas365splines)
                                }
                                
                                rjd3sts::add(model_sts, rjd3sts::noise("n", variance = self$noise_variance, fixed = FALSE))
                                
                                rslt_sts <- rjd3sts::estimate(
                                  model_sts,
                                  pre_pro$model$linearized,
                                  initialization = "SqrtDiffuse",
                                  optimizer = self$optimizer,
                                  concentrated = TRUE,
                                  precision = self$precision
                                )
                                
                                sc <- rjd3sts::smoothed_components(rslt_sts, fast = FALSE)
                                
                                # serie destagionalizzata: linearized - trend - stagionali
                                res<-pre_pro$model$linearized - sc[,2] - sc[,3]
                                
                                # --- re-add LS ---
                                if(is_ls_present){

                                    res <- res + pre_pro$model$component_ls          # scala già additiva/naturale, somma diretta
                                  
                                }
                                # ---
                                
                                if(self$log_pre_pro)
                                {
                                  res <- exp(res)
                                }  
                                res
                                
                              }
                            )
)


# sts_estimator <- STSEstimator$new(data = data)
# results_sts <- calculate_mar_convergence_optimized(
#   estimator_instance = sts_estimator,
#   lag = 365
# )
# results_sts
# 
# sts_estimator2 <- STSEstimator$new(data = data)
# results_sts2 <- calculate_revisions_by_lags(
#   estimator_instance = sts_estimator2
# )
# results_sts2
# # STL: MAR_percent_mean=0.15
# # STL_multi: MAR_percent_mean=2.40
# # X11: MAR_percent_mean=1.28
# # STS: MAR_percent_mean=1.71





################################### AMB
AMBEstimator <- R6::R6Class("AMBEstimator",
                            inherit = SignalEstimator,
                            public = list(
                              data = NULL,
                              x_pre_pro = NULL,
                              cal_reg_pre_pro = NULL,
                              dates_by_pre_pro = NULL,
                              dates_from_pre_pro = NULL,
                              criticalValue_pre_pro = NULL,
                              periods_pre_pro = NULL,
                              log_pre_pro = NULL,
                              outliers_pre_pro = NULL,
                              period_s1 = NULL,
                              period_s2 = NULL,
                              log_amb = NULL,
                              last_pre_pro = NULL,   # <-- AGGIUNGI QUESTA RIGA
                              
                              initialize = function(data,
                                                    # ==== Parametri pre_processing ====
                                                    x_pre_pro = NULL,
                                                    cal_reg_pre_pro      = NA,
                                                    dates_by_pre_pro     = "day",
                                                    dates_from_pre_pro   = "2012-01-01",
                                                    criticalValue_pre_pro = 6,
                                                    periods_pre_pro      = c(7, 365.2425),
                                                    log_pre_pro          = FALSE,
                                                    outliers_pre_pro     = c("ao","wo", "ls"),
                                                    # ==== Parametri AMB ====
                                                    period_s1 = 7,
                                                    period_s2 = 365.2425,
                                                    log_amb   = FALSE) {
                                
                                super$initialize(data)
                                
                                self$data <- data
                                self$x_pre_pro            <- x_pre_pro
                                self$cal_reg_pre_pro      <- cal_reg_pre_pro
                                self$dates_by_pre_pro     <- dates_by_pre_pro
                                self$dates_from_pre_pro   <- dates_from_pre_pro
                                self$criticalValue_pre_pro <- criticalValue_pre_pro
                                self$periods_pre_pro      <- periods_pre_pro
                                self$log_pre_pro          <- log_pre_pro
                                self$outliers_pre_pro     <- outliers_pre_pro
                                self$period_s1            <- period_s1
                                self$period_s2            <- period_s2
                                self$log_amb              <- log_amb
                              },
                              
                              run = function() {
                                require(rjd3sts)
                                require(rjd3toolkit)
                                require(rjd3highfreq)
                                
                                # ---- pre_processing parametrico
                                pre_pro <- pre_processing(
                                  data          = self$data,
                                  x_pre_pro     = adjust_x_pre_pro(self$x_pre_pro, self$data$Value),
                                  cal_reg       = self$cal_reg_pre_pro,
                                  dates_by      = self$dates_by_pre_pro,
                                  dates_from    = self$dates_from_pre_pro,
                                  criticalValue = self$criticalValue_pre_pro,
                                  periods       = self$periods_pre_pro,
                                  log           = self$log_pre_pro,
                                  outliers      = self$outliers_pre_pro
                                )
                                
                                self$last_pre_pro <- pre_pro
                                
                                is_ls_present <- !is.null(pre_pro$model$component_ls) && 
                                  length(pre_pro$model$component_ls) > 0 && 
                                  !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls))
                                
                                # ---- decomposizione FRACTIONAL AIRLINE
                                amb_dow <- rjd3highfreq::fractionalAirlineDecomposition(
                                  y = pre_pro$model$linearized,
                                  period = self$period_s1,
                                  log = FALSE, #self$log_amb, # already done by pre_processing routine
                                  y_time = self$data$Date
                                )
                                
                                amb_doy <- rjd3highfreq::fractionalAirlineDecomposition(
                                  y = amb_dow$decomposition$sa,
                                  period = self$period_s2,
                                  log = FALSE, #self$log_amb,
                                  y_time = self$data$Date
                                )
                                
                                # --- re-add LS ---
                                if(is_ls_present){

                                    amb_doy$decomposition$sa <- amb_doy$decomposition$sa + pre_pro$model$component_ls  
                                    
                                  
                                }
                                # ---
                                
                                if(self$log_pre_pro==TRUE)
                                {
                                  #amb.doy$decomposition$sa <- exp(amb.doy$decomposition$sa)
                                  amb_doy$decomposition$sa <- exp(amb_doy$decomposition$sa)
                                  
                                }
                                
                                amb_doy$decomposition$sa
                              }
                            )
)

# amb_estimator <- AMBEstimator$new(data = data)
# results_amb <- calculate_mar_convergence_optimized(
#   estimator_instance = amb_estimator,
#   lag = 365
# )
# results_amb
# 
# amb_estimator2 <- AMBEstimator$new(data = data)
# results_amb2 <- calculate_revisions_by_lags(
#   estimator_instance = amb_estimator2
# )
# results_amb2
# # STL: MAR_percent_mean=0.15
# # STL_multi: MAR_percent_mean=2.40
# # X11: MAR_percent_mean=1.28
# # STS: MAR_percent_mean=1.71
# # AMB: MAR_percent_mean=1.93







################################### AMB MULTI
AMBEstimatorMulti <- R6::R6Class("AMBEstimatorMulti",
                                 inherit = SignalEstimator,
                                 public = list(
                                   # === campi ===
                                   data = NULL,
                                   x_pre_pro = NULL,
                                   cal_reg_pre_pro = NULL,
                                   dates_by_pre_pro = NULL,
                                   dates_from_pre_pro = NULL,
                                   criticalValue_pre_pro = NULL,
                                   periods_pre_pro = NULL,
                                   log_pre_pro = NULL,
                                   outliers_pre_pro = NULL,
                                   periods_amb = NULL,
                                   log_amb = NULL,
                                   last_pre_pro = NULL,   # <-- AGGIUNGI QUESTA RIGA
                                   
                                   initialize = function(data,
                                                         # ==== Parametri pre_processing ====
                                                         x_pre_pro = NULL,
                                                         cal_reg_pre_pro      = NA,
                                                         dates_by_pre_pro     = "day",
                                                         dates_from_pre_pro   = "2012-01-01",
                                                         criticalValue_pre_pro = 6,
                                                         periods_pre_pro      = c(7,365.2425),
                                                         log_pre_pro          = FALSE,
                                                         outliers_pre_pro     = c("ao","wo", "ls"),
                                                         # ==== Parametri multiAirline ====
                                                         periods_amb = c(7, 365.2425),
                                                         log_amb     = FALSE) {
                                     
                                     super$initialize(data)
                                     
                                     self$data <- data
                                     self$x_pre_pro            <- x_pre_pro
                                     self$cal_reg_pre_pro      <- cal_reg_pre_pro
                                     self$dates_by_pre_pro     <- dates_by_pre_pro
                                     self$dates_from_pre_pro   <- dates_from_pre_pro
                                     self$criticalValue_pre_pro <- criticalValue_pre_pro
                                     self$periods_pre_pro      <- periods_pre_pro
                                     self$log_pre_pro          <- log_pre_pro
                                     self$outliers_pre_pro     <- outliers_pre_pro
                                     self$periods_amb          <- periods_amb
                                     self$log_amb              <- log_amb
                                   },
                                   
                                   run = function() {
                                     require(rjd3sts)
                                     require(rjd3toolkit)
                                     require(rjd3highfreq)
                                     
                                     # ---- pre_processing parametrico
                                     pre_pro <- pre_processing(
                                       data          = self$data,
                                       x_pre_pro     = adjust_x_pre_pro(self$x_pre_pro, self$data$Value),
                                       cal_reg       = self$cal_reg_pre_pro,
                                       dates_by      = self$dates_by_pre_pro,
                                       dates_from    = self$dates_from_pre_pro,
                                       criticalValue = self$criticalValue_pre_pro,
                                       periods       = self$periods_pre_pro,
                                       log           = self$log_pre_pro,
                                       outliers      = self$outliers_pre_pro
                                     )
                                     
                                     self$last_pre_pro <- pre_pro
                                     
                                     is_ls_present <- !is.null(pre_pro$model$component_ls) && 
                                       length(pre_pro$model$component_ls) > 0 && 
                                       !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls))
                                     
                                     # ---- decomposizione multiAirline
                                     amb_multi <- rjd3highfreq::multiAirlineDecomposition(
                                       y       = pre_pro$model$linearized,
                                       periods = self$periods_amb,
                                       log     = FALSE, # self$log_amb, # Already done by the preprocessing routine
                                       y_time  = self$data$Date
                                     )
                                     
                                     # --- re-add LS ---
                                     if(is_ls_present)
                                     {

                                         amb_multi$decomposition$sa <- amb_multi$decomposition$sa + pre_pro$model$component_ls  
                                          
                                       
                                     }
                                     # ---
                                     
                                     if(self$log_pre_pro==TRUE)
                                     {
                                       amb_multi$decomposition$sa <- exp(amb_multi$decomposition$sa)
                                     }
                                     
                                     amb_multi$decomposition$sa
                                   }
                                 )
)


# amb_estimator_multi <- AMBEstimatorMulti$new(data = data)
# results_amb_multi <- calculate_mar_convergence_optimized(
#   estimator_instance = amb_estimator_multi,
#   lag = 365
# )
# results_amb_multi
# 
# amb_estimator_multi2 <- AMBEstimatorMulti$new(data = data)
# results_amb_multi2 <- calculate_revisions_by_lags(
#   estimator_instance = amb_estimator_multi2
# )
# results_amb_multi2
# 
# # STL: MAR_percent_mean=0.15
# # STL_multi: MAR_percent_mean=2.40
# # X11: MAR_percent_mean=1.28
# # STS: MAR_percent_mean=1.71
# # AMB: MAR_percent_mean=1.93
# # AMB_multi: MAR_percent_mean=2.75





################################### AMB MULTI
TBATSEstimator <- R6::R6Class("TBATSEstimator",
                              inherit = SignalEstimator,
                              public = list(
                                # === campi ===
                                data = NULL,
                                x_pre_pro = NULL,
                                cal_reg_pre_pro = NULL,
                                dates_by_pre_pro = NULL,
                                dates_from_pre_pro = NULL,
                                criticalValue_pre_pro = NULL,
                                periods_pre_pro = NULL,
                                log_pre_pro = NULL,
                                outliers_pre_pro = NULL,
                                periods_tbats = NULL,
                                use_boxcox = NULL,
                                log_tbats = NULL,
                                last_pre_pro = NULL,   # <-- AGGIUNGI QUESTA RIGA
                                
                                initialize = function(data,
                                                      # ==== Parametri pre_processing ====
                                                      x_pre_pro = NULL,
                                                      cal_reg_pre_pro      = NA,
                                                      dates_by_pre_pro     = "day",
                                                      dates_from_pre_pro   = "2012-01-01",
                                                      criticalValue_pre_pro = 6,
                                                      periods_pre_pro      = c(7,365.2425),
                                                      log_pre_pro          = FALSE,
                                                      outliers_pre_pro     = c("ao","wo", "ls"),
                                                      # ==== Parametri multiAirline ====
                                                      periods_tbats = c(7, 365.2425),
                                                      use_boxcox    = FALSE,
                                                      log_tbats     = FALSE)
                                {
                                  
                                  super$initialize(data)
                                  
                                  self$data <- data
                                  self$x_pre_pro            <- x_pre_pro
                                  self$cal_reg_pre_pro      <- cal_reg_pre_pro
                                  self$dates_by_pre_pro     <- dates_by_pre_pro
                                  self$dates_from_pre_pro   <- dates_from_pre_pro
                                  self$criticalValue_pre_pro <- criticalValue_pre_pro
                                  self$periods_pre_pro      <- periods_pre_pro
                                  self$log_pre_pro          <- log_pre_pro
                                  self$outliers_pre_pro     <- outliers_pre_pro
                                  self$periods_tbats        <- periods_tbats
                                  self$use_boxcox           <- use_boxcox
                                  self$log_tbats            <- log_tbats
                                },
                                
                                run = function() {
                                  # require(rjd3sts)
                                  # require(rjd3toolkit)
                                  # require(rjd3highfreq)
                                  # 
                                  # # ---- pre_processing parametrico
                                  # pre_pro <- pre_processing(
                                  #   data          = self$data,
                                  #   x_pre_pro     = adjust_x_pre_pro(self$x_pre_pro, self$data$Value),
                                  #   cal_reg       = self$cal_reg_pre_pro,
                                  #   dates_by      = self$dates_by_pre_pro,
                                  #   dates_from    = self$dates_from_pre_pro,
                                  #   criticalValue = self$criticalValue_pre_pro,
                                  #   periods       = self$periods_pre_pro,
                                  #   log           = self$log_pre_pro,
                                  #   outliers      = self$outliers_pre_pro
                                  # )
                                  # 
                                  # # ---- decomposizione multiAirline
                                  # amb_multi <- rjd3highfreq::multiAirlineDecomposition(
                                  #   y       = pre_pro$model$linearized,
                                  #   periods = self$periods_amb,
                                  #   log     = FALSE, # self$log_amb, # Already done by the preprocessing routine
                                  #   y_time  = self$data$Date
                                  # )
                                  # 
                                  # if(self$log_pre_pro==TRUE)
                                  # {
                                  #   amb_multi$decomposition$sa <- exp(amb_multi$decomposition$sa)
                                  # }
                                  # 
                                  # amb_multi$decomposition$sa
                                  
                                  
                                  
                                  # # ---- pre_processing parametrico
                                  pre_pro <- pre_processing(
                                    data          = self$data,
                                    x_pre_pro     = adjust_x_pre_pro(self$x_pre_pro, self$data$Value),
                                    cal_reg       = self$cal_reg_pre_pro,
                                    dates_by      = self$dates_by_pre_pro,
                                    dates_from    = self$dates_from_pre_pro,
                                    criticalValue = self$criticalValue_pre_pro,
                                    periods       = self$periods_pre_pro,
                                    log           = self$log_pre_pro,
                                    outliers      = self$outliers_pre_pro
                                  )
                                  
                                  self$last_pre_pro <- pre_pro
                                  
                                  is_ls_present <- !is.null(pre_pro$model$component_ls) && 
                                    length(pre_pro$model$component_ls) > 0 && 
                                    !(length(pre_pro$model$component_ls) == 1 && is.na(pre_pro$model$component_ls))
                                  
                                  library(forecast)
                                  
                                  
                                  # browser()
                                  
                                  fit_tbats <- tbats(
                                    pre_pro$model$linearized,
                                    seasonal.periods = c(7, 365),
                                    use.box.cox = FALSE,
                                  )
                                  
                                  
                                  # 1. Estrarre la scomposizione dal modello
                                  tbats_components <- tbats.components(fit_tbats)
                                  
                                  
                                  S               <- c()
                                  sa_tbats        <- c()
                                  irregular_tbats <- c()
                                  
                                  if(self$log_pre_pro)
                                  {
                                    # log-scale
                                    S_log     <- tbats_components[,"season1"] + tbats_components[,"season2"]
                                    # T_log     <- tbats_components[,"level"]   + tbats_components[,"slope"]
                                    sa_log    <- tbats_components[,"observed"] - S_log
                                    
                                    # --- Re-add LS ---
                                    if(is_ls_present){
                                        sa_log <- sa_log + pre_pro$model$component_ls
                                    }
                                    # ---
                                    
                                    # irr_log   <- tbats_components[,"observed"] - (T_log + S_log)
                                    
                                    # natural scale
                                    S               <- exp(S_log)
                                    # trend_tbats     <- exp(T_log)
                                    sa_tbats        <- exp(sa_log)
                                    # irregular_tbats <- exp(irr_log)
                                  }else
                                  {
                                    S        <- tbats_components[,"season1"] + tbats_components[,"season2"]
                                    sa_tbats <- pre_pro$model$linearized - S
                                    
                                    # --- Re-add LS ---
                                    if(is_ls_present){
                                      sa_tbats <- sa_tbats + pre_pro$model$component_ls}
                                    # ---
                                    
                                    #  irregular_tbats <- sa_tbats - tbats_components[,"level"] #- tbats_components[,"slope"]
                                    #  trend_tbats     <- tbats_components[,"level"] #+ tbats_components[,"slope"]
                                    
                                  }  
                                  
                                  
                                  
                                  # if(log)
                                  # {
                                  #   sa_tbats <- exp(sa_tbats)
                                  # }else {
                                  #   sa_tbats <- sa_tbats 
                                  # }
                                  
                                  
                                  return(sa_tbats)
                                  
                                }
                              )
)















# library(tidyverse)
# 
# # install.packages("remotes")
# # remotes::install_github("rjdverse/rjd3revisions@*release", build_vignettes = TRUE)
# revision_dataframe=data.frame(rev_date=as.Date("2021-12-31"),
#                               time_period=data$Date,
#                               obs_values=doy_stl$decomposition[,'sa'])
# glimpse(revision_dataframe)
# library("rjd3revisions")
# vintages_1 <- create_vintages(x = revision_dataframe, type = "long", periodicity = 365)
