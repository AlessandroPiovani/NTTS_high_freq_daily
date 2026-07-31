############################### seats diagnostics ##############################

# diagnostics_seas_irr <- function(seas_component, irr_component, original_series) {
#   seas_variance  <- var(seas_component, na.rm = TRUE)
#   irr_variance   <- var(irr_component, na.rm = TRUE)
#   total_variance <- var(original_series, na.rm = TRUE)
#   # Variances in proportion to the total variance of the original series
#   prop_seas <- seas_variance / total_variance
#   prop_irr <- irr_variance / total_variance
#   cross_correlation <- cor(seas_component, irr_component, use = "complete.obs")
#   result <- data.frame(
#     seas_variance   = seas_variance,
#     irr_variance    = irr_variance,
#     total_variance  = total_variance,
#     proportion_seas = prop_seas,
#     proportion_irr  = prop_irr,
#     cross_correlation = cross_correlation
#   )
#   return(result)
# }


########################### seasonality tests ##################################
run_seasonality_tests <- function(data, periods = c(7, 30, 365)) {
  results <- data.frame(Test     = character(),
                        Period   = numeric(),
                        P_Value  = numeric(),
                        Presence = character(),
                        stringsAsFactors = FALSE)
  
  for (p in periods) {
    s <- do_stationary(data, period = p)$ddata
    
    # Test QS -> Seasonal Ljung-Box
    qs_test <- seasonality_qs(s)
    results <- rbind(results, data.frame(Test = "QS", Period = p,
                                         P_Value = format(round(qs_test$pvalue, 5), scientific = FALSE),  # Arrotondamento e no scientific
                                         Presence = ifelse(qs_test$pvalue < 0.05, "PRESENT", "NONE")))
                                         # Presence = ifelse(qs_test$pvalue < 0.10, "PRESENT", "NONE")))
    
    # Test  Kruskal-Wallis
    kw_test <- seasonality_kruskalwallis(s, period = p)
    results <- rbind(results, data.frame(Test = "Kruskal-Wallis", Period = p,
                                         P_Value = format(round(kw_test$pvalue, 5), scientific = FALSE),
                                         Presence = ifelse(kw_test$pvalue < 0.05, "PRESENT", "NONE")))
                                         # Presence = ifelse(kw_test$pvalue < 0.10, "PRESENT", "NONE")))
    
    # Test Periodogram
    periodogram_test <- seasonality_periodogram(s, period = p)
    results <- rbind(results, data.frame(Test = "Periodogram", Period = p,
                                         P_Value = format(round(periodogram_test$pvalue, 5), scientific = FALSE),
                                         Presence = ifelse(periodogram_test$pvalue < 0.05, "PRESENT", "NONE")))
                                         # Presence = ifelse(periodogram_test$pvalue < 0.10, "PRESENT", "NONE")))
    
    # Test Friedman
    friedman_test <- seasonality_friedman(s, period = p)
    results <- rbind(results, data.frame(Test = "Friedman", Period = p,
                                         P_Value = format(round(friedman_test$pvalue, 5), scientific = FALSE),
                                         Presence = ifelse(friedman_test$pvalue < 0.05, "PRESENT", "NONE")))
                                         # Presence = ifelse(friedman_test$pvalue < 0.10, "PRESENT", "NONE")))
    
    # Test F
    f_test <- seasonality_f(data, period = p, model = c("AR", "D1", "WN"))
    results <- rbind(results, data.frame(Test = "F-Test", Period = p,
                                         P_Value = format(round(f_test$pvalue, 5), scientific = FALSE),
                                         Presence = ifelse(f_test$pvalue < 0.05, "PRESENT", "NONE")))
                                         # Presence = ifelse(f_test$pvalue < 0.10, "PRESENT", "NONE")))
    
    # Test Canova-Hansen (Contrasts)
    ch_contrast_test <- seasonality_canovahansen(data, p, type = "Contrast")$seasonality
    results <- rbind(results, data.frame(Test = "Canova-Hansen Contrast", Period = p,
                                         P_Value = format(round(ch_contrast_test$pvalue, 5), scientific = FALSE),
                                         Presence = ifelse(ch_contrast_test$pvalue < 0.05, "PRESENT", "NONE")))
                                         # Presence = ifelse(ch_contrast_test$pvalue < 0.10, "PRESENT", "NONE")))
    
    # Test Canova-Hansen (Trigonometric)
    ch_trig_test <- seasonality_canovahansen(data, p, type = "Trigonometric")$seasonality
    results <- rbind(results, data.frame(Test = "Canova-Hansen Trigonometric", Period = p,
                                         P_Value = format(round(ch_trig_test$pvalue, 5), scientific = FALSE),
                                         Presence = ifelse(ch_trig_test$pvalue < 0.05, "PRESENT", "NONE")))
                                         # Presence = ifelse(ch_trig_test$pvalue < 0.10, "PRESENT", "NONE")))
    
    
    # Combined Test (only presence , p-value NA)
    combined_test <- seasonality_combined(s, period = p, firstperiod = cycle(data)[1], mul = TRUE)$seasonality
    results <- rbind(results, data.frame(Test = "Combined", Period = p,
                                         P_Value = NA,
                                         Presence = combined_test))
  }
  
  return(results)
}




seasonality_summary <- function(results_df) {
  require(dplyr)
  
  summary_df <- results_df %>%
    group_by(Period) %>%
    summarise(
      tests_signaling_seasonality = paste(Test[Presence == "PRESENT"], collapse = ", "), 
      # summary = ifelse(sum(Presence == "PRESENT") >= sum(Presence == "NONE"), "PRESENT", "NONE")
      # summary = ifelse(sum(Presence == "PRESENT") >= 4, "PRESENT", "NONE")
      n_presences=(sum(Presence == "PRESENT"))
      
    )
  
  return(as.data.frame(summary_df))
}



################################ ljung box ####################################
run_ljung_box_tests <- function(preprocessing_res_list, lag_periodicities, times_periodicites_to_compute_k = 4) {
  
  results_list <- list()
  
  # Loop over each preprocessing result (i.e., each dataset or model output)
  for (i in seq_along(preprocessing_res_list)) {
    pre_res <- preprocessing_res_list[[i]]  # Extract the current preprocessing result
    model_name <- names(preprocessing_res_list)[i]  # Get the name of the current model
    
    # Get the number of hyperparameters from the preprocessing result's likelihood information
    nhp <- pre_res[["likelihood"]][["nparams"]]
    
    periods <- pre_res[["model"]][["periods"]]
    
    #k <- max(periods) * times_periodicites_to_compute_k  # Use the max period if k_values is not provided
    
    # Perform Ljung-Box tests for different periodicity lags
    for (season in names(lag_periodicities)) {
      for (lag in lag_periodicities[[season]]) {
        
        
        # browser()
        k <- min(lag_periodicities[[season]]) * times_periodicites_to_compute_k  # Use the min periodicity of the lags which should be the same periodicity you want to test
        
        
        # Run Ljung-Box test for residuals of the current preprocessing result's model
        test_result <- ljungbox(pre_res$model$residuals, 
                                k = k, lag = lag, nhp = nhp, 
                                sign = 0, mean = TRUE)
        
        # Extract value and p-value
        test_value <- test_result[["value"]]
        p_value <- test_result[["pvalue"]]
        
        # Format p-value: truncate to 5 decimal places and avoid scientific notation
        formatted_p_value <- formatC(p_value, format = "f", digits = 5)
        
        # Determine independence classification
        independence <- ifelse(p_value >= 0.05, "INDEPENDENCE", "NO_INDEP")
        
        # Append result as a row in the list
        results_list[[length(results_list) + 1]] <- data.frame(
          Model = model_name,
          Period = season,
          Lag = lag,
          Statistic = test_value,
          P_Value = formatted_p_value,
          Independence = independence,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  # Convert list of data frames into a single data frame
  results_df <- do.call(rbind, results_list)
  
  return(results_df)
}


print_seasonality_tests <- function(test_results, alpha = 0.05, digits = 4) {
  
  for (test in names(test_results)) {
    
    cat("\n========================================\n")
    cat("Test:", test, "\n")
    
    df <- as.data.frame(test_results[[test]])
    
    # Applichiamo la regola corretta a seconda del tipo di test
    if (test == "CanovaHansen") {
      cat("Stable seasonal periods (p >= ", alpha, ")\n", sep = "")
      cat("========================================\n")
      sig <- df[df$pvalue >= alpha, c("value", "pvalue"), drop = FALSE]
    } else {
      # Per F-Test e KruskalWallis cerchiamo p < alpha
      cat("Significant seasonal periods (p < ", alpha, ")\n", sep = "")
      cat("========================================\n")
      sig <- df[df$pvalue < alpha, c("value", "pvalue"), drop = FALSE]
    }
    
    if (nrow(sig) == 0) {
      cat("No periods matching the criteria.\n")
    } else {
      print(round(sig, digits))
      cat("\n")
      cat("Total matched:", nrow(sig), "out of", nrow(df), "periods.\n")
    }
  }
}












######################## weekend and week days regressors ######################
generate_weekend_regressor <- function(start_date, end_date) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  weekend_matrix <- as.matrix(
    as.integer(weekdays(dates) %in% c("sabato", "domenica"))
  )
  
  rownames(weekend_matrix) <- format(dates, "%Y-%m-%d")
  
  return(weekend_matrix)
}


generate_weekdays_regressors <- function(start_date, end_date) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  weekday_matrix <- matrix(0, nrow = length(dates), ncol = 6)
  
  rownames(weekday_matrix) <- format(dates, "%Y-%m-%d")
  
  colnames(weekday_matrix) <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
  
  for (i in 1:length(dates)) {
    day_of_week <- weekdays(dates[i])
    
    if (day_of_week == "lunedì") {
      weekday_matrix[i, "monday"] <- 1
    } else if (day_of_week == "martedì") {
      weekday_matrix[i, "tuesday"] <- 1
    } else if (day_of_week == "mercoledì") {
      weekday_matrix[i, "wednesday"] <- 1
    } else if (day_of_week == "giovedì") {
      weekday_matrix[i, "thursday"] <- 1
    } else if (day_of_week == "venerdì") {
      weekday_matrix[i, "friday"] <- 1
    } else if (day_of_week == "sabato") {
      weekday_matrix[i, "saturday"] <- 1
    }
  }
  
  return(weekday_matrix)
}


plot_seasonal_spectrum <- function(tests_list, max_period = 365) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Helper interno per estrarre e pulire i dati da ogni test safely
  extract_test_data <- function(df, test_name) {
    if (is.null(df)) return(NULL)
    
    df <- as.data.frame(df)
    
    # Identifica dove si trova l'informazione sulla periodicità/frequenza
    if ("period" %in% colnames(df)) {
      df$Periodo <- as.numeric(df$period)
    } else if ("frequency" %in% colnames(df)) {
      df$Periodo <- as.numeric(df$frequency)
    } else {
      # Fallback sui row.names se non ci sono colonne esplicite
      df$Periodo <- as.numeric(row.names(df))
    }
    
    df$Test <- test_name
    return(df)
  }
  
  # 1. Estrazione e unione dei test presenti nella lista
  raw_extracted <- list(
    extract_test_data(tests_list[["CanovaHansen"]], "Canova-Hansen"),
    extract_test_data(tests_list[["F"]], "F-Test"),
    extract_test_data(tests_list[["KruskalWallis"]], "Kruskal-Wallis")
  )
  
  plot_data <- bind_rows(raw_extracted)
  
  if (nrow(plot_data) == 0) {
    stop("La lista dei test fornita è vuota o non formattata correttamente.")
  }
  
  # 2. Filtraggio logico in base alla definizione statistica di "evidenza"
  # F-Test e Kruskal-Wallis: p-value < 0.05 (rifiuto assenza di stagionalità)
  # Canova-Hansen: p-value >= 0.05 (manteniamo l'ipotesi di stagionalità stabile)
  plot_data_filtered <- plot_data %>% 
    filter(
      (Test %in% c("F-Test", "Kruskal-Wallis") & pvalue < 0.05) |
        (Test == "Canova-Hansen" & pvalue >= 0.05)
    ) %>% 
    filter(Periodo >= 0 & Periodo <= max_period)
  
  if (nrow(plot_data_filtered) == 0) {
    warning("Nessuna statistica test è risultata significativa secondo i criteri impostati.")
    return(NULL)
  }
  
  # 3. Generazione del grafico ad alta risoluzione spettrale
  gg <- ggplot(plot_data_filtered, aes(x = Periodo, y = value, color = Test)) +
    geom_segment(aes(xend = Periodo, yend = 0), size = 0.8, alpha = 0.8) + 
    geom_point(size = 2.5) +
    facet_wrap(~Test, scales = "free_y", ncol = 1) + 
    scale_x_continuous(
      breaks = c(7, 14, 30, 91, 182, 365),
      labels = c("7d\n(Sett)", "14d", "30d\n(Mese)", "91d\n(Trim)", "182d\n(Sem)", "365d\n(Anno)")
    ) + 
    labs(
      title = "Spettro delle Periodicità Stagionali Significative",
      subtitle = "Evidenza di stagionalità: p-value < 0.05 per F e KW; p-value >= 0.05 per Canova-Hansen",
      x = "Periodicità Rilevata",
      y = "Valore della Statistica Test"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", size = 10, color = "grey40", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 9, vjust = 0.5),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
  
  return(gg)
}


######################à heteroskedasticity ##################################
non_parametric_White_test <- function(pre_pro,y,log=F, plot=F){
  diff <- length(y)-length(pre_pro$model$residuals)+1
  if(log==T){
    y <- log(y)
  } 
  fitted_val <- y + pre_pro$model$residuals
  aux_model <- lm(log(pre_pro$model$residuals^2) ~ fitted_val + I(fitted_val^2))
  
  if(plot==T){
    plot(fitted_val, pre_pro$model$residuals)
  }
  
  return(summary(aux_model))
  
}

non_parametric_White_test_amb <- function(pre_pro,y,log=F, plot=F){
  diff <- length(y)-length(pre_pro$model$residuals)+1
  if(log==T){
    y <- log(y)
  } 
  fitted_val <- y + pre_pro$decomposition$i
  aux_model <- lm(log(pre_pro$decomposition$i^2) ~ fitted_val + I(fitted_val^2))
  
  if(plot==T){
    plot(fitted_val, pre_pro$decomposition$i)
  }
  
  return(summary(aux_model))
  
}

non_parametric_White_test_amb_log <- function(pre_pro,y,log=F, plot=F){
  diff <- length(y)-length(pre_pro$model$residuals)+1
  if(log==T){
    y <- log(y)
  } 
  fitted_val <- exp(y) + exp(pre_pro$decomposition$i)
  aux_model <- lm(log(exp(pre_pro$decomposition$i)^2) ~ fitted_val + I(fitted_val^2))
  
  if(plot==T){
    plot(fitted_val, exp(pre_pro$decomposition$i))
  }
  
  return(summary(aux_model))
  
}

non_parametric_White_test_sts <- function(sc,y,log=F, plot=F){
  diff <- length(y)-length(sc[,4])+1
  if(log==T){
    y <- log(y)
  } 
  fitted_val <- y + sc[,4]
  aux_model <- lm(log(sc[,4]^2) ~ fitted_val + I(fitted_val^2))
  
  if(plot==T){
    plot(fitted_val, sc[,4])
  }
  
  return(summary(aux_model))
  
}

non_parametric_White_test_sts_log <- function(sc,y,log=F, plot=F){
  diff <- length(y)-length(sc[,4])+1
  if(log==T){
    y <- log(y)
  } 
  fitted_val <- exp(y) + exp(sc[,4])
  aux_model <- lm(log(exp(sc[,4])^2) ~ fitted_val + I(fitted_val^2))
  
  if(plot==T){
    plot(fitted_val, exp(sc[,4]))
  }
  
  return(summary(aux_model))
  
}


non_parametric_White_test_stl <- function(rslt_stl_y,y,log=F, plot=F){
  diff <- length(y)-length(rslt_stl_y$decomposition[,"i"])+1
  if(log==T){
    y <- log(y)
  } 
  fitted_val <- y + rslt_stl_y$decomposition[,"i"]
  aux_model <- lm(log(rslt_stl_y$decomposition[,"i"]^2) ~ fitted_val + I(fitted_val^2))
  
  if(plot==T){
    plot(fitted_val, rslt_stl_y$decomposition[,"i"])
  }
  
  return(summary(aux_model))
  
}

non_parametric_White_test_stl_log <- function(rslt_stl_y,y,log=F, plot=F){
  diff <- length(y)-length(rslt_stl_y$decomposition[,"i"])+1
  if(log==T){
    y <- log(y)
  } 
  fitted_val <- exp(y) + exp(rslt_stl_y$decomposition[,"i"])
  aux_model <- lm(log(exp(rslt_stl_y$decomposition[,"i"])^2) ~ fitted_val + I(fitted_val^2))
  
  if(plot==T){
    plot(fitted_val, exp(rslt_stl_y$decomposition[,"i"]))
  }
  
  return(summary(aux_model))
  
}


hmc_test <- function(pre_pro) {
  res <- pre_pro$model$residuals
  n <- length(res)
  R <- rank(abs(res))
  rho <- cor(R, 1:n)
  return(rho)
}

hmc_test_amb <- function(pre_pro) {
  res <- pre_pro$decomposition$i
  n <- length(res)
  R <- rank(abs(res))
  rho <- cor(R, 1:n)
  return(rho)
}

hmc_test_amb_log <- function(pre_pro) {
  res <- exp(pre_pro$decomposition$i)
  n <- length(res)
  R <- rank(abs(res))
  rho <- cor(R, 1:n)
  return(rho)
}

hmc_test_stl <- function(rslt_stl_y) {
  res <- rslt_stl_y$decomposition[,"i"]
  n <- length(res)
  R <- rank(abs(res))
  rho <- cor(R, 1:n)
  return(rho)
}

hmc_test_stl_log <- function(rslt_stl_y) {
  res <- exp(rslt_stl_y$decomposition[,"i"])
  n <- length(res)
  R <- rank(abs(res))
  rho <- cor(R, 1:n)
  return(rho)
}



diagnostics_seas_irr <- function(seas_component, irr_component, trend_component, original_series, label=NA) {
  # 1. Variance Analysis
  seas_variance  <- var(seas_component, na.rm = TRUE)
  irr_variance   <- var(irr_component, na.rm = TRUE)
  trend_variance <- var(trend_component, na.rm = TRUE)
  total_variance <- var(original_series, na.rm = TRUE)
  
  # Proporzioni della varianza
  prop_seas  <- seas_variance  / total_variance
  prop_irr   <- irr_variance   / total_variance
  prop_trend <- trend_variance / total_variance
  
  # 2. Cross-Correlation and p-value
  cross_correlation <- suppressWarnings(cor(seas_component, irr_component, use = "complete.obs"))
  cross_corr_p_value <- tryCatch({
    test <- cor.test(seas_component, irr_component, use = "complete.obs")
    as.numeric(test$p.value)
  }, error = function(e) NA_real_)
  
  # 3. Ljung-Box Test
  ljung_box <- tryCatch({
    test <- Box.test(irr_component, lag = 10, type = "Ljung-Box")
    list(
      statistic = as.numeric(test$statistic),
      p_value   = as.numeric(test$p.value)
    )
  }, error = function(e) list(statistic = NA_real_, p_value = NA_real_))
  
  # 4. Shapiro-Wilk Test
  shapiro <- tryCatch({
    test <- shapiro.test(irr_component)
    list(
      statistic = as.numeric(test$statistic),
      p_value   = as.numeric(test$p.value)
    )
  }, error = function(e) list(statistic = NA_real_, p_value = NA_real_))
  
  # 5. Reconstruction Error (MAE)
  recomposed_series <- seas_component + irr_component + trend_component
  mae <- mean(abs(original_series - recomposed_series), na.rm = TRUE)
  mape <- mae/sum(original_series)*100
  
  # 6. ACF Lag-1 of Residuals after Seasonal Adjustment
  residuals_seas_adjusted <- original_series - seas_component
  seas_adjusted <- residuals_seas_adjusted
  
  acf_val <- tryCatch({
    acf_res <- acf(seas_adjusted, plot = T, lag.max = 500)
    as.numeric(acf_res$acf[8])  # lag 1
  }, error = function(e) NA_real_)
  
  # 7. Return full result
  result <- list(
    seas_variance           = seas_variance,
    irr_variance            = irr_variance,
    trend_variance          = trend_variance,
    total_variance          = total_variance,
    proportion_seas         = prop_seas,
    proportion_irr          = prop_irr,
    proportion_trend        = prop_trend,
    cross_correlation       = cross_correlation,
    cross_corr_p_value      = cross_corr_p_value,
    ljung_box_statistic     = ljung_box$statistic,
    ljung_box_p_value       = ljung_box$p_value,
    shapiro_statistic       = shapiro$statistic,
    shapiro_p_value         = shapiro$p_value,
    mae_reconstruction      = mae,
    mape_reconstruction     = mape, 
    acf_residuals_lag1      = acf_val,
    acf_res = acf_res
  )
  
  return(result)
}




#################################################################################
calculate_mar_convergence_optimized <- function(y, estimator_instance, lag, delta_lag = floor(lag / 4)) {
  if (delta_lag <= 0) stop("delta_lag must be > 0")
  T <- length(y)
  
  # Usa direttamente l'istanza dell'estimator (modifica principale)
  estimator_full <- estimator_instance  # Ora non usi più new(), ma l'istanza direttamente
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
    
    # Usa l'istanza e aggiorna il dato (modifica importante)
    estimator_vintage <- estimator_instance  # Usa la stessa istanza
    estimator_vintage$y <- y_vintage  # Modifica i dati nell'istanza
    
    S_vintage <- estimator_vintage$run()
    
    common_t <- 1:(vintage_end - 1)
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
  
  results$MAR_mean <- mean(results$MAR, na.rm = TRUE)
  results$MAR_percent_mean <- mean(results$MAR_percent, na.rm = TRUE)
  
  # Plot
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    plot <- ggplot(results, aes(x = lag_values)) +
      #geom_line(aes(y = MAR, color = "MAR")) +
      geom_line(aes(y = MAR_percent, color = "MAR (%)")) +
      labs(title = "MAR and MAR (%) over Increasing Lags",
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





################# Metterli in un file a parte e fare source ####################

library(R6)

# CLasse generica che obbliga ad avere il metodo Run
SignalEstimator <- R6Class("SignalEstimator",
                           public = list(
                             y = NULL,
                             initialize = function(y) {
                               self$y <- y
                             },
                             run = function() {
                               stop("run() must be implemented in a subclass")
                             }
                           )
)

# Specializzazione STL estimator, tailored su STL: dentro initialize mettere tutti gli argomenti che servono
STLEstimator_test <- R6Class("STLEstimator_test",
                        inherit = SignalEstimator,
                        public = list(
                          frequency = NULL,
                          initialize = function(y, frequency = 365) {
                            super$initialize(y)
                            self$frequency <- frequency
                          },
                          run = function() {  # personalizzarla a seconda del metodo
                            ts_data <- ts(self$y, frequency = self$frequency)
                            stl_fit <- stats::stl(ts_data, s.window = "periodic", robust = TRUE)
                            return(as.numeric(stl_fit$time.series[, "trend"]))
                          }
                        )
)


# Specializzazione STL estimator, tailored su STL: dentro initialize mettere tutti gli argomenti che servono
X11Estimator_test <- R6Class("X11Estimator_test",
                        inherit = SignalEstimator,
                        public = list(
                          frequency = NULL,
                          other = NULL,
                          initialize = function(y, frequency = 365, other=999999) {
                            super$initialize(y)
                            self$frequency <- frequency
                            self$other     <- other
                          },
                          run = function() {  # personalizzarla a seconda del metodo
                            ts_data <- ts(self$y, frequency = self$frequency)
                            stl_fit <- stats::stl(ts_data, s.window = "periodic", robust = TRUE)
                            return(as.numeric(stl_fit$time.series[, "trend"]))
                          }
                        )
)








################################## Chiamate ####################################

# set.seed(123)
# t <- 1:2500
# trend <- 0.01 * t
# seasonality <- sin(2 * pi * t / 365)
# noise <- rnorm(2500, mean = 0, sd = 0.3)
# y <- trend + seasonality + noise
# 
# stl_estimator_test <- STLEstimator_test$new(y = y, frequency = 365)
# x11_estimator_test <- X11Estimator_test$new(y = y, frequency = 365, other = 999999)
# 
# 
# results <- calculate_mar_convergence_optimized(
#   y = y,
#   estimator_instance = stl_estimator_test,
#   lag = 365
# )
# 
# 
# results <- calculate_mar_convergence_optimized(
#   y = y,
#   estimator_instance = x11_estimator_test,
#   lag = 365
# )
# 


analyze_irregular <- function(irregular_component, name="Series") {
  library(lmtest)
  library(ggplot2)
  cat("\n=== Analyzing irregular component:", name, "===\n\n")
  
  # ---- Homoscedasticity test ----
  cat("Test 1: Homoscedasticity (Breusch-Pagan)\n")
  cat("  H0: Variance of residuals is constant (homoscedastic)\n")
  cat("  H1: Variance of residuals is not constant (heteroscedastic)\n\n")
  
  lm_model <- lm(irregular_component ~ seq_along(irregular_component))
  bp_test <- bptest(lm_model)
  print(bp_test)
  
  # ---- Zero mean test ----
  cat("\nTest 2: Zero mean (t-test)\n")
  cat("  H0: Mean of residuals is 0\n")
  cat("  H1: Mean of residuals is not 0\n\n")
  
  t_test <- t.test(irregular_component, mu = 0)
  print(t_test)
  
  # ---- Distribution plot ----
  p1 <- ggplot(data.frame(resid=irregular_component), aes(x=resid)) +
    geom_histogram(aes(y=..density..), bins=30, fill="skyblue", color="black") +
    geom_density(color="red", size=1) +
    ggtitle(paste("Distribution of irregular component:", name)) +
    xlab("Residual values") + ylab("Density")
  print(p1)
  
  # ---- ACF plot ----
  acf_res <- tryCatch({
    acf(irregular_component, plot = TRUE, lag.max = 500, main=paste("ACF of irregular component:", name))
  }, error = function(e) NA)
  
  # ---- PACF plot ----
  pacf_res <- tryCatch({
    pacf(irregular_component, plot = TRUE, lag.max = 500, main=paste("PACF of irregular component:", name))
  }, error = function(e) NA)

  
  # ---- Q-Q plot ----
  qqnorm(irregular_component, main=paste("Q-Q plot of irregular component:", name))
  qqline(irregular_component, col="red", lwd=2)
  
  # ---- Randomness tests ----
  cat("\nTest 4: Randomness (Runs tests)\n")
  
  # 4a. Runs around median
  cat("\n4a. Runs around median\n")
  cat("  H0: Sequence is random\n")
  cat("  H1: Sequence is not random\n\n")
  runs_median <- tryCatch({
    testofruns(irregular_component)
  }, error = function(e) NA)
  print(runs_median)
  
  
  # ---- Return lag-1 values for reference ----
  acf_val  <- if (inherits(acf_res , "acf")) as.numeric( acf_res$acf[2]) else NA_real_
  pacf_val <- if (inherits(pacf_res, "acf")) as.numeric(pacf_res$acf[2]) else NA_real_
  
  return(list(
    bptest = bp_test,
    ttest = t_test,
    acf_lag1 = acf_val,
    pacf_lag1 = pacf_val,
    acf = acf_res,
    pacf= pacf_res,
    runs_median = runs_median
  ))
}


run_tests <- function(series, data_name = "original", 
                      test_name = c("CanovaHansen", "F", "KruskalWallis")) {
  
  periods_test <- 2:367
  results <- list()
  
  if ("CanovaHansen" %in% test_name) {
    cat("Canova-Hansen test \nH0: no deterministic seasonality. ",
        "P-value < 0.05 --> Deterministic seasonality\n")
    for (i in periods_test) {
      result <- seasonality_canovahansen(series, i, type = "Trigonometric")
      cat("Analysis:", data_name,
          "| Period:", i,
          "| p-value:", round(result$seasonality$pvalue, 4),
          "| value:", round(result$seasonality$value, 4), "\n")
      results[[length(results) + 1]] <- data.frame(
        analysis = data_name,
        test = "Canova-Hansen",
        period = i,
        pvalue = result$seasonality$pvalue,
        value  = result$seasonality$value
      )
    }
  }
  
  if ("F" %in% test_name) {
    cat("Pointwise F-test \nH0: no seasonality at period i. ",
        "P-value < 0.05 --> Significant seasonality\n")
    for (i in periods_test) {
      result <- seasonality_f(series, i)
      cat("Analysis:", data_name,
          "| Period:", i,
          "| p-value:", round(result$pvalue, 4),
          "| value:", round(result$value, 4), "\n")
      results[[length(results) + 1]] <- data.frame(
        analysis = data_name,
        test = "F-test",
        period = i,
        pvalue = result$pvalue,
        value  = result$value
      )
    }
  }
  
  if ("KruskalWallis" %in% test_name) {
    cat("Pointwise Kruskal-Wallis test \nH0: no seasonality at period i. ",
        "P-value < 0.05 --> Significant seasonality\n")
    for (i in periods_test) {
      result <- seasonality_kruskalwallis(series, i)
      cat("Analysis:", data_name,
          "| Period:", i,
          "| p-value:", round(result$pvalue, 4),
          "| value:", round(result$value, 4), "\n")
      results[[length(results) + 1]] <- data.frame(
        analysis = data_name,
        test = "Kruskal-Wallis",
        period = i,
        pvalue = result$pvalue,
        value  = result$value
      )
    }
  }
  
  do.call(rbind, results)
}


create_TLS_matrix <- function(date_start, date_end, TLS_start, TLS_end, name = "TLS") {
  # Sequenza di date
  dates <- seq.Date(as.Date(date_start), as.Date(date_end), by = "day")
  
  # Vettore valori TLS (0 fuori dall'intervallo, 1 dentro)
  TLS <- ifelse(dates >= as.Date(TLS_start) & dates <= as.Date(TLS_end), 1, 0)
  
  # Costruzione matrice
  M <- matrix(TLS, ncol = 1)
  colnames(M) <- name
  rownames(M) <- format(dates, "%Y-%m-%d")
  
  return(M)
}




aggregate_daily_to_monthly <- function(data, fun = mean) {
  stopifnot("Date" %in% names(data), "Value" %in% names(data))
  
  # assicurati che Date sia in formato Date
  data$Date <- as.Date(data$Date)
  
  # ricava anno-mese
  ym <- format(data$Date, "%Y-%m")
  
  # aggrega per mese usando la funzione scelta (default = mean)
  monthly <- tapply(data$Value, ym, fun, na.rm = TRUE)
  
  # ricava date mensili (primo giorno del mese)
  months <- as.Date(paste0(names(monthly), "-01"))
  
  # crea serie temporale in formato data.frame
  df_monthly <- data.frame(Date = months, Value = as.numeric(monthly))
  
  # converte in ts (richiesto da rjd3)
  start_year <- as.numeric(format(min(df_monthly$Date), "%Y"))
  start_month <- as.numeric(format(min(df_monthly$Date), "%m"))
  ts_monthly <- ts(df_monthly$Value, start = c(start_year, start_month), frequency = 12)
  
  list(data = df_monthly, ts = ts_monthly)
}


sa_all_methods <- function(res_monthly) {
  

  library(forecast)
  y <- res_monthly$ts
  dates <- res_monthly$data$Date
  

  # TRAMO-SEATS (RSAfull)
  ts_sa <- rjd3tramoseats::tramoseats(y, spec = "RSAfull")
  sa_ts <- ts_sa[["result"]][["final"]][["sa"]][["data"]]
  
  # X13 (RSA5c = specifica più completa)
  x13_sa <- rjd3x13::x13(y, spec = "RSA5c")
  sa_x13 <- ts_sa[["result"]][["final"]][["sa"]][["data"]]
  
  # STL
  stl_sa <- rjd3stl::istl(y, periods = 12)
  sa_stl <- stl_sa$decomposition[,"sa"]
  
  # STS
  tramomdl <- rjd3tramoseats::tramo(y, spec = "RSAfull")
  if(tramomdl$result$description$log){  y_lin <- exp(tramomdl$result$estimation$y) }
  else { y_lin <- tramomdl$result$estimation$y }
  
  y_lin <- ts(y_lin, start = dates[1], frequency = 12)
  
  sts_sa <- rjd3sts::sts(y_lin)
  sa_sts <- y_lin - sts_sa$decomposition$seasonal$data
  
  # TBATS 
  tbats_sa <- tbats(y, seasonal.periods = 12) # Box-Cox testata    
  
  # browser()
  tbats_components <- tbats.components(tbats_sa)
  S_tbats  <- tbats_components[,"season"]
  sa_tbats <- y - S_tbats    
 
  # struttura finale
  SA <- data.frame(
    date = dates,
    TRAMO_SEATS = as.numeric(sa_ts),
    X13 = as.numeric(sa_x13),
    STL = as.numeric(sa_stl),
    STS = as.numeric(sa_sts),
    TBATS =  as.numeric(sa_tbats)
  )
  
  return(SA)
}




plot_kde_acf <- function(raw_data = NULL, sa_data = NULL, 
                         lags = c(7, 365, 30), window_cycles = c(4, 2, 3), 
                         title_label = NULL, show_legend = TRUE,
                         xlim_min = NULL, xlim_max = NULL,
                         ylim_min = NULL, ylim_max = NULL,
                         custom_colors = NULL) {
  
  if (is.null(raw_data) & is.null(sa_data)) 
    stop("Provide at least one series (raw_data or sa_data).")
  if (length(lags) > 3)
    stop("Provide at most three lags.")
  if (length(window_cycles) != length(lags))                          # <-- NUOVO controllo
    stop("`window_cycles` must have the same length as `lags`.")
  
  library(ggplot2)
  library(dplyr)
  library(stats)
  library(RColorBrewer)
  
  acf_samples <- function(x, lag, window_cycles) {
    window <- window_cycles * lag
    n <- length(x)
    if (n <= window || n <= lag) {
      message(paste0("⚠️  Skipped lag ", lag, ": window (", window, 
                     ") too short for series length ", n))
      return(numeric(0))
    }
    vals <- numeric(0)
    for (i in seq(1, n - window)) {
      segment <- x[i:(i + window - 1)]
      acf_val <- tryCatch(acf(segment, lag.max = lag, plot = FALSE)$acf[lag + 1],
                          error = function(e) NA)
      vals <- c(vals, acf_val)
    }
    vals[is.finite(vals)]
  }
  
  df_list <- list()
  
  if (!is.null(raw_data)) {
    for (i in seq_along(lags)) {          # <-- CAMBIATO: for (L in lags)
      L  <- lags[i]                       # <-- NUOVO
      wc <- window_cycles[i]              # <-- NUOVO: finestra specifica per questo lag
      acf_vals <- acf_samples(raw_data, L, wc)   # <-- CAMBIATO: wc invece di window_cycles
      if (length(acf_vals) > 0) {
        df_list[[paste0("raw_", L)]] <- data.frame(
          value = acf_vals,
          lag = paste0("Lag ", L),
          type = "Raw data"
        )
      } else {
        message(paste0("No valid ACF values for Raw data (lag = ", L, ")."))
      }
    }
  }
  
  if (!is.null(sa_data)) {
    for (i in seq_along(lags)) {          # <-- CAMBIATO: stesso schema del blocco raw
      L  <- lags[i]
      wc <- window_cycles[i]
      acf_vals <- acf_samples(sa_data, L, wc)
      if (length(acf_vals) > 0) {
        df_list[[paste0("sa_", L)]] <- data.frame(
          value = acf_vals,
          lag = paste0("Lag ", L),
          type = "Seasonally adjusted"
        )
      } else {
        message(paste0("No valid ACF values for SA data (lag = ", L, ")."))
      }
    }
  }
  
  # --- da qui in poi tutto invariato rispetto a prima ---
  df_all <- bind_rows(df_list)
  if (nrow(df_all) == 0) 
    stop("No valid ACF values available to plot. Try reducing 'window_cycles' or using smaller lags.")
  
  df_all$lag <- factor(df_all$lag, levels = paste0("Lag ", lags))
  lag_levels <- max(3, length(unique(df_all$lag)))
  
  if (!is.null(custom_colors)) {
    if (length(custom_colors) < length(unique(df_all$lag))) {
      warning("I colori personalizzati forniti sono inferiori al numero di Lag nel grafico. Uso il default.")
      colors <- RColorBrewer::brewer.pal(lag_levels, "Set1")[1:length(unique(df_all$lag))]
    } else {
      colors <- custom_colors[1:length(unique(df_all$lag))]
    }
  } else {
    colors <- RColorBrewer::brewer.pal(lag_levels, "Set1")[1:length(unique(df_all$lag))]
  }
  names(colors) <- levels(df_all$lag)[1:length(unique(df_all$lag))]
  
  if (!is.null(title_label)) {
    title_lab <- title_label
  } else {
    title_lab <- "KDE of rolling sample ACF"
  }
  
  p <- ggplot(df_all, aes(x = value, color = lag, linetype = type)) +
    annotate("rect", xmin = -0.025, xmax = 0.025, ymin = 0, ymax = Inf,
             fill = "grey80", alpha = 0.4) +
    geom_density(linewidth = 1.1) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = c("Raw data" = "solid",
                                     "Seasonally adjusted" = "dashed")) +
    labs(
      x = "ACF value",
      y = "Density",
      title = title_lab,
      subtitle = paste("Lags:", paste(lags, collapse = ", ")),
      color = "Lag",
      linetype = "Series"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    ) +
    coord_cartesian(xlim = c(xlim_min, xlim_max),
                    ylim = c(ylim_min, ylim_max))
  
  if (!show_legend) p <- p + theme(legend.position = "none")
  return(p)
}






analyze_spectral_peaks <- function(x, is_spectrum = FALSE, max_peaks = 15) {
  library(dplyr)
  
  # 1. Gestione dell'input: calcola lo spettro se viene passata la serie grezza
  if (!is_spectrum) {
    message("Calcolo del periodogramma in corso...")
    spec_obj <- spec.pgram(x, log = FALSE, plot = FALSE)
    spec_df <- data.frame(freq = spec_obj$freq, spec = spec_obj$spec)
  } else {
    # Se passi direttamente l'oggetto generato da spec.pgram
    spec_df <- data.frame(freq = x$freq, spec = x$spec)
  }
  
  # 2. Funzione interna geometrica per identificare i massimi locali (i picchi)
  find_peaks <- function(y) {
    # Trova i punti in cui la derivata prima cambia segno da positiva a negativa
    pks <- which(diff(sign(diff(y))) == -2) + 1
    return(pks)
  }
  
  # 3. Estrazione dei picchi e filtraggio (solo quelli sopra la media per evitare il rumore)
  peak_indices <- find_peaks(spec_df$spec)
  mean_spec <- mean(spec_df$spec)
  
  peaks_filtered <- spec_df[peak_indices, ] %>% 
    filter(spec > mean_spec)
  
  # 4. Calcolo del periodo in giorni (1 / frequenza) e formattazione
  peaks_output <- peaks_filtered %>%
    mutate(Periodo_Giorni = 1 / freq) %>%
    arrange(desc(spec)) %>%
    head(max_peaks)
  
  # 5. Ritorna il dataframe pulito senza indici di riga intrinseci
  return(as.data.frame(peaks_output, row.names = NULL))
}


generate_eom_regressors <- function(data_dates, num_days_before = 0) {
  # 1. Convertiamo l'input in un vettore di oggetti Date pulito
  dates <- as.Date(data_dates)
  n <- length(dates)
  
  # 2. Identifichiamo geometricamente l'ultimo giorno del mese (EOM)
  is_eom <- format(dates, "%m") != format(dates + 1, "%m")
  is_eom[n] <- TRUE # Forza l'ultimo record assoluto
  
  # 3. Inizializziamo la matrice dei regressori per l'ultimo giorno
  eom_matrix <- matrix(as.integer(is_eom), ncol = 1,
                       dimnames = list(as.character(dates), "end_of_month"))
  
  # 4. Aggiungiamo i regressori per i giorni precedenti, se richiesto
  if (num_days_before > 0) {
    for (k in 1:num_days_before) {
      # Creiamo un vettore vuoto di zeri
      is_eom_minus_k <- rep(0, n)
      
      # Per ogni giorno che è fine mese, impostiamo a 1 il giorno che si trova 'k' posizioni prima
      # Usiamo un ciclo sicuro per evitare indici fuori scala (< 1)
      eom_indices <- which(is_eom)
      target_indices <- eom_indices - k
      target_indices <- target_indices[target_indices >= 1]
      
      is_eom_minus_k[target_indices] <- 1
      
      # Creiamo la colonna con un nome esplicito
      col_name <- paste0("eom_minus_", k)
      col_matrix <- matrix(as.integer(is_eom_minus_k), ncol = 1,
                           dimnames = list(as.character(dates), col_name))
      
      # Uniamo la colonna alla matrice principale
      eom_matrix <- cbind(eom_matrix, col_matrix)
    }
  }
  
  return(eom_matrix)
}


weekdays_components_plot <- function(ts_list, ts_names) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  
  # browser()
  
  # Verifica che le lunghezze siano coerenti
  if(length(ts_list) != length(ts_names)) stop("ts_list e ts_names devono avere la stessa lunghezza")
  
  # Creiamo un dataframe unificato
  df_all <- data.frame()
  
  for(i in seq_along(ts_list)) {
    ts_data <- ts_list[[i]]
    ts_name <- ts_names[i]
    
    # Assumiamo che ts_data sia un oggetto ts giornaliero
    ts_df <- data.frame(
      date = seq.Date(from = start(ts_data), by = "day", length.out = length(ts_data)),
      value = as.numeric(ts_data),
      series = ts_name
    )
    
    ts_df$weekday <- weekdays(ts_df$date)
    
    df_all <- rbind(df_all, ts_df)
  }
  
  # Calcola la media per ogni giorno della settimana
  df_summary <- df_all %>%
    group_by(series, weekday) %>%
    summarize(mean_val = mean(value, na.rm = TRUE), .groups = "drop")
  
  # Ordina i giorni della settimana
  df_all$weekday <- factor(df_all$weekday, 
                           levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  df_summary$weekday <- factor(df_summary$weekday, 
                               levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  
  # Plot
  ggplot(df_all, aes(x = 1, y = value, group = series, color = series)) +
    geom_line(aes(x = as.numeric(date), y = value)) +  # linee delle varie serie
    geom_hline(data = df_summary, aes(yintercept = mean_val, color = series), linetype = "solid", size = 0.8) +
    facet_wrap(~weekday, ncol = 7, scales = "fixed") +
    theme_minimal() +
    labs(x = "", y = "Value", color = "Series") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

# Esempio di uso:
# ts_list <- list(ts1, ts2, ts3, ts4, ts5, ts6)
# ts_names <- c("WeeklyTS", "WeeklyX12", "WeeklySTL", "WeeklySSM", "Avg", "Extra")
# plot_ts_groups(ts_list, ts_names)

exponentiate_components <- function(sa_result) {
  
  components <- sa_result$decomposition
  
  if (is.list(components)) {
    components <- lapply(components, function(x) {
      if (is.numeric(x)) exp(x) else x
    })
    sa_result$decomposition <- components
    return(sa_result)
  } else{ return(NULL) }
  
}




compute_si_daily <- function(log=TRUE, s7, i, dates) {
  
  # browser()
  
  # Check lunghezze
  if(!(length(s7) == length(i) & length(i) == length(dates))) {
    stop("s7, i and dates must have the same length")
  }
  
  # Converti le date
  dates <- as.Date(dates)
  if(any(is.na(dates))) stop("Date format not recognized, use YYYY-MM-DD")
  
  # Giorno della settimana (1 = lunedì, 7 = domenica)
  dow <- weekdays(dates, abbreviate = FALSE)
  
  # Calcolo S–I ratio (moltiplicativo) o difference (additivo)
  if (log) {

    i[i==0]  <- 1
    s7[i==0] <- 1
    
    # modello moltiplicativo
    si_value <- s7 * i # s7 / i
    measure <- "ratio"
  } else {
    # modello additivo
    si_value <- s7 + i # s7 - i
    measure <- "difference"
  }
  
  # Data frame completo
  df <- data.frame(
    date = dates,
    dow = factor(dow,
                 levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
    s7 = s7,
    i = i,
    si = si_value
  )
  
  # Riepilogo per giorno della settimana
  
  
  library(dplyr)
  
  summary_dow <- df %>%
    group_by(dow) %>%
    summarise(
      mean_si = mean(si),
      sd_si = sd(si)
    )
  
  # summary_dow <- aggregate(si ~ dow, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))
  # 
  # # Sistemo la struttura (split delle colonne mean/sd)
  # summary_dow <- do.call(data.frame, summary_dow)
  # names(summary_dow) <- c("dow", "mean_si", "sd_si")
  
  # Risultato
  return(
    list(
      measure = measure,
      daily = df,
      by_weekday = summary_dow
    )
  )
}

plot_si_weekly <- function(dates, si, 
                           s7 = NA,
                           main = NA, 
                           log_ratio = FALSE,
                           y_min = NA,
                           y_max = NA,
                           show = c("points","boxplot"),
                           plot_y_lab=TRUE) {
  # browser()
  show <- match.arg(show)
  
  # Controlli
  dates <- as.Date(dates)
  if (length(dates) != length(si)) stop("dates and si must have the same length")
  
  # Log ratio con gestione valori <= 0
  if(log_ratio) {
    min_si <- min(si)
    if(min_si <= 0) {
      offset <- abs(min_si) + 1e-6
      si <- si + offset
      message("S–I values adjusted by offset = ", round(offset,6), " to compute log")
    }
    si <- log(si)
    
    if(is.na(main))
    {  
      main <- "Log S–I Plot (Weekly)"
    }  
  } else{
    if(is.na(main))
    { 
       main<-"S–I Plot (Weekly)"
    }   
  }
  
  # Giorno della settimana
  dow <- factor(weekdays(dates),
                levels = c("Monday","Tuesday","Wednesday","Thursday","Friday",
                           "Saturday","Sunday"))
  
  # Calcolo media
  mean_dow <- tapply(si, dow, mean)
  
  # Colori
  col_bg <- "white"
    col_lines <- rgb(0.8, 0.8, 0.8)
    
    # Impostazioni grafiche
    #op <- par(no.readonly=TRUE)
    #on.exit(par(op))
    
    if(plot_y_lab==FALSE)
    {
      par(bg = col_bg, mar = c(2.5,2,1.5,0.5))
    }else
    {
      par(bg = col_bg, mar = c(4,4,4,2))
    }  
        
    # Mapping 1..7
    x_numeric <- as.numeric(dow)
    
    if(plot_y_lab)
    {  
      if(log_ratio){
        label_y <- "log(S–I)"
      }else{
        label_y <- "S–I"
      }  
    }else{
      label_y <- ""
    }
      
    
    # Plot vuoto con limiti y opzionali
    plot(x_numeric, si, type="n", xaxt="n", xlab="", 
         ylab = label_y,
         main=main, xlim=c(0.5, 7.5),
         ylim = if(is.na(y_min) || is.na(y_max)) NULL else c(y_min, y_max))
    
    # Linee verticali
    for (k in 0:7) abline(v = k + 0.5, col = col_lines)
    
    # --- Visualizzazione dei dati ---
    if(show == "points") {
      # Punti
      points(x_numeric, si, pch=20, cex=0.7)
    } else if(show == "boxplot") {
      # Boxplot per ciascun giorno della settimana
      boxplot(si ~ dow, add = TRUE, axes = FALSE,
              col = rgb(0,0,1,0.2), border = rgb(0,0,1),
              at = 1:7, width = rep(0.6, 7))
    }
    
    # Smoothed LOESS o stagionalità per ogni giorno
    for(day in levels(dow)) {
      idx <- which(dow == day)
      ord <- order(dates[idx])
      
      y_vals <- si[idx][ord]
      x_time <- as.numeric(dates[idx][ord])
      
      
      if(length(s7)==1 && is.na(s7))# if s7 is not available, use a LOESS
      {
        fit <- stats::loess(y_vals ~ x_time, span = 0.3)
        y_smooth <- predict(fit, x_time) 
      }else  
      {
        y_smooth <- s7[idx][ord]  # seasonal component passed as input
      }
      

      n <- length(idx)
      x_local <- seq(-0.3, 0.3, length.out = n)
      
      day_col <- which(levels(dow) == day)
      x_plot <- day_col + x_local
      
      lines(x_plot, y_smooth, col="blue", lwd=2)
    }
    
    # Segmenti medi rossi
    for (k in 1:7) {
      y_mean <- mean_dow[k]
      segments(k - 0.4, y_mean, k + 0.4, y_mean, col="red", lwd=1)
    }
    
    # Etichette asse X
    axis(1, at=1:7, labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
}



# --- FUNZIONE PER I PANNELLI ---
# Accetta sia i dati originali (per media e SD)
# sia quelli standardizzati (per l'istogramma)
plot_hist_panel <- function(irr_data_raw, irr_data_std, title_text) {
  
  # Salva le impostazioni grafiche correnti e le ripristina all'uscita
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  # Aumenta il margine sinistro per evitare che "Density" venga tagliato
  par(mar = c(5.1, 6.8, 4.1, 2.1))
  
  # Calcolo statistiche descrittive sui dati originali
  raw_clean <- irr_data_raw[!is.na(irr_data_raw)]
  avg_orig  <- mean(raw_clean)
  sd_orig   <- sd(raw_clean)
  
  # Istogramma dei dati standardizzati
  hist(
    irr_data_std,
    breaks = 40,
    freq = FALSE,
    col = "lightblue",
    border = "white",
    main = "",
    xlab = "Standardized value",
    ylab = "Density",
    cex.axis = 1.6,
    cex.lab = 1.6
  )
  
  # Curva della Normale standard
  curve(
    dnorm(x, mean = 0, sd = 1),
    col = "red",
    lwd = 2.5,
    add = TRUE,
    n = 300
  )
  
  # Testo con media e deviazione standard originali
  stats_text <- paste0(
    "Mean (orig): ", formatC(avg_orig, digits = 4, format = "g"), "\n",
    "SD (orig): ", formatC(sd_orig, digits = 4, format = "g")
  )
  
  # Stampa delle statistiche nell'angolo in alto a destra
  usr <- par("usr")
  text(
    x = usr[2] - 0.02 * diff(usr[1:2]),
    y = usr[4] - 0.02 * diff(usr[3:4]),
    labels = stats_text,
    adj = c(1, 1),
    cex = 1.2
  )
  
  # Titolo del pannello
  title(
    main = title_text,
    font.main = 2,
    cex.main = 1.8
  )
}


# ---------------------------------------------------------------------------
# Funzioni di supporto
# ---------------------------------------------------------------------------

# Applica l'aggiustamento per l'effetto calendario (moltiplicativo o additivo)
add_cal_eff <- function(log, time_varying_cal_eff, data) {
  if (log) {
    data * time_varying_cal_eff
  } else {
    data + time_varying_cal_eff
  }
}

# Filtra un data.frame su una colonna data tra start e end (inclusi)
filter_range <- function(df, date_col, start, end) {
  df %>%
    dplyr::filter(.data[[date_col]] >= as.Date(start),
                  .data[[date_col]] <= as.Date(end))
}


# Grafico di confronto "raw / sa_ca daily / sa_ca daily->monthly / SA mensile"
# per un singolo metodo di destagionalizzazione.
# sa_vector : vettore giornaliero della componente sa del metodo (stesso ordine di data$Date)
# sa_column : nome della colonna in SA (data.frame con le 4 destagionalizzazioni mensili) da usare come riferimento
# title     : titolo del grafico
# legend_label : etichetta per la curva viola in legenda
plot_sa_ca_comparison <- function(sa_vector,
                                  sa_column,
                                  title,
                                  legend_label,
                                  log,
                                  series = c("SA_CA", "SA"),
                                  cal_effect = data$Time_Varying) {
  
  series <- match.arg(series)
  
  # sa_vector (es. amb.doy$decomposition$sa) è già la serie con calendario E
  # stagionalità rimossi (perché la pipeline a monte parte da una serie già
  # calendar-adjusted). Quindi:
  #  - "SA_CA" (Seasonally-Calendar Adjusted): è sa_vector così com'è, senza
  #    aggiungere nulla — entrambi gli effetti sono già fuori.
  #  - "SA" (solo Seasonally Adjusted): bisogna REINTRODURRE il calendario,
  #    perché "SA" per definizione ha solo la stagionalità tolta, non il
  #    calendario.
  plot_series <- if (series == "SA_CA") {
    sa_vector
  } else {
    add_cal_eff(log, cal_effect, sa_vector)
  }
  
  df <- data.frame(
    date  = data$Date,
    sa    = sa_vector,
    plot  = plot_series
  ) %>%
    filter_range("date", year_start, sa_end_extra)
  
  df_monthly <- df %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(
      monthly = mean(plot, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_days <- filter_range(df, "date", year_start, year_end)
  
  par(blank_par)
  on.exit(par(blank_par), add = TRUE)
  
  layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
  par(mar = c(5, 4, 4, 2))
  
  plot(data2$Date, data2$Value,
       type = "l", col = "grey70",
       xlab = "Time", ylab = label_original,
       lwd = 1, xaxt = "n")
  
  title(main = title, font.main = 1)
  
  axis.Date(1,
            at = seq(min(data2$Date), max(data2$Date), by = "month"),
            format = "%b")
  
  lines(df_days$date, df_days$plot,
        col = "orange", lwd = 2)
  
  lines(df_monthly$month, df_monthly$monthly,
        col = "red", lwd = 2,
        type = "b", pch = 17)
  
  lines(SA$date, SA[[sa_column]],
        col = "purple", lwd = 2,
        type = "b", pch = 16)
  
  par(mar = c(0, 0, 0, 0))
  plot.new()
  
  legend_daily <- if (series == "SA_CA") "SA & CA daily" else "SA daily"
  legend_month <- if (series == "SA_CA") {
    "SA & CA daily → monthly mean"
  } else {
    "SA daily → monthly mean"
  }
  
  legend(
    "center",
    legend = c(
      "Raw (daily)",
      legend_daily,
      legend_month,
      legend_label
    ),
    col = c("grey70", "orange", "red", "purple"),
    lty = 1,
    lwd = 2,
    pch = c(NA, NA, 17, 16),
    bty = "n",
    cex = 1.1
  )
}



produce_outputs <- function(data,
                            sa_daily,
                            calendar_effect) {
  
  library(dplyr)
  library(lubridate)
  
  # -----------------------------
  # Preparazione dati giornalieri
  # -----------------------------
  
  daily_df <- data %>%
    arrange(Date) %>%
    mutate(
      sa = as.numeric(sa_daily),
      cal_effect = as.numeric(calendar_effect),
      sa_ca = sa + cal_effect,
      month = floor_date(Date, unit = "month")
    )
  
  
  # -----------------------------
  # Serie grezza mensile
  # -----------------------------
  
  raw_monthly <- daily_df %>%
    group_by(month) %>%
    summarise(
      raw = mean(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # -----------------------------
  # Serie destagionalizzata giornaliera
  # -----------------------------
  
  sa_daily_out <- daily_df %>%
    select(
      Date,
      sa
    )
  
  
  # -----------------------------
  # Serie SA con calendario riaggiunto
  # riaggregata mensilmente
  # -----------------------------
  
  sa_ca_monthly <- daily_df %>%
    group_by(month) %>%
    summarise(
      sa_ca = mean(sa_ca, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  return(
    list(
      raw_monthly = raw_monthly,
      sa_daily = sa_daily_out,
      sa_ca_monthly = sa_ca_monthly
    )
  )
}
