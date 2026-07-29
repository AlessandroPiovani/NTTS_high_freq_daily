# stability_analysis_eurostat <- function(data, estimator_class, span_length = 3*365, step = 365, tau = 0.03) {
#   n <- nrow(data)
#   spans <- list()
#   
#   # --- 4 sliding spans ---
#   for (i in 0:3) {
#     start <- 1 + i*step
#     end <- min(start + span_length - 1, n)
#     subset <- data[start:end, ]
#     sa <- estimator_class$new(data = subset)$run()
#     spans[[i+1]] <- data.frame(date = subset$Date, sa = sa)
#   }
#   
#   # --- tutte le date presenti in almeno uno span ---
#   all_dates <- sort(unique(unlist(lapply(spans, function(x) x$date))))
#   
#   # --- costruzione matrice sa ---
#   sa_matrix <- sapply(spans, function(sp) {
#     y <- rep(NA_real_, length(all_dates))
#     idx <- match(sp$date, all_dates)
#     y[idx] <- sp$sa
#     y
#   })
#   
#   # --- funzione per calcolare range sicuro tra almeno due valori ---
#   safe_range <- function(x) {
#     vals <- x[!is.na(x)]
#     if(length(vals) < 2) return(NA_real_)
#     (max(vals) - min(vals)) / min(vals)
#   }
#   
#   # --- X% ---
#   X_row <- apply(sa_matrix, 1, safe_range)
#   X <- mean(X_row > tau, na.rm = TRUE) * 100
#   
#   # --- MM, QQ, YY con lag ---
#   calc_shift <- function(h) {
#     diffs <- sapply(1:(nrow(sa_matrix)-h), function(i) {
#       safe_range(sa_matrix[i, ] - sa_matrix[i+h, ])
#     })
#     mean(diffs > tau, na.rm = TRUE) * 100
#   }
#   
#   MM <- calc_shift(30)    # circa 1 mese
#   QQ <- calc_shift(91)    # circa 1 trimestre
#   YY <- calc_shift(365)   # circa 1 anno
#   
#   data.frame(X = X, MM = MM, QQ = QQ, YY = YY)
# }



stability_analysis <- function(data, estimator, span_length = 9*365, tau = 0.03) {
  # Costruzione dei 4 spans in stile Findley
  n <- nrow(data)
  spans <- list()
  for (i in 0:3) {
    start <- 1 + i*122
    end   <- start + span_length - 1
    if (end <= n) {
      subset <- data[start:end, ]
      sa <- estimator$new(data = subset)$run()
      spans[[i+1]] <- data.frame(date = subset$Date, sa = sa)
      colnames(spans[[i+1]])[2] <- paste0("sa", i+1)  # <-- nome unico
    }
  }
  
  merged <- Reduce(function(x,y) merge(x,y, by="date", all=TRUE), spans)
  
  # Funzione per calcolare la variazione relativa ignorando NA
  safe_range <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 2) return(NA_real_)  # servono almeno due spans
    mn <- min(x)
    if (mn <= 0) return(NA_real_)        # evita divisione per zero/valori negativi
    (max(x) - mn) / mn
  }
  
  # Calcolo X%
  X <- mean(apply(merged[,-1], 1, safe_range) > tau, na.rm=TRUE) * 100
  
  # Calcolo MM%, QQ%, YY%
  sa_matrix <- as.matrix(merged[,-1])   # togli la colonna delle date
  
  calc_shift <- function(h) {
    if (nrow(sa_matrix) <= h) return(NA_real_)
    rel_ranges <- sapply(1:(nrow(sa_matrix)-h), function(i) {
      d <- sa_matrix[i + h, ] - sa_matrix[i, ]
      safe_range(d)
    })
    mean(rel_ranges > tau, na.rm = TRUE) * 100
  }
  
  
  MM <- calc_shift(30)
  QQ <- calc_shift(91)
  YY <- calc_shift(365)
  
  
  return(data.frame(X = X, MM = MM, QQ = QQ, YY = YY))
}













######## STL


#stab_results <- stability_analysis(data, STLSEQEstimator, tau = 0.03)
#print(stab_results)





######## STL_multi


#stab_results <- stability_analysis(data, STLEstimatorMulti, tau = 0.03)
#print(stab_results)




######## X11



#stab_results <- stability_analysis(data, X11Estimator, tau = 0.03)
#print(stab_results)



######## STS

#stab_results <- stability_analysis(data, STSEstimator, tau = 0.03)
#print(stab_results)

######## AMB


#stab_results <- stability_analysis(data, AMBEstimator, tau = 0.03)
#print(stab_results)


######## AMB multi
#stab_results <- stability_analysis(data, AMBEstimatorMulti, tau = 0.03)
#print(stab_results)







