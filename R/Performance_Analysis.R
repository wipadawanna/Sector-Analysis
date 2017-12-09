

plot_model_performance <- function(input_xts, train_windows, predict_window, 
                                   result_set, alpha = NA, accuracy){
  N <- nrow(input_xts)
  data_length <-  N - train_windows +1
  if(is.na(alpha)){
    plot(y = input_xts[, "beat_mkt"][train_windows:N], 
         x = index(input_xts)[train_windows:N], 
         typ = "l", pch = 19, main = paste("Rolling", train_windows, "months for next", 
                                           predict_window, "months"),
         ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
    
  }else{
    plot(y = input_xts[, "beat_mkt"][train_windows:N], 
         x = index(input_xts)[train_windows:N], 
         typ = "l", pch = 19, main = paste("Rolling", train_windows, 
                                           "months:Robust for next", 
                                           predict_window, "months with alpha = ", alpha),
         ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
  }
  lines(y = rep(0.5, data_length), x = index(input_xts)[train_windows:N], col = "green")
  points(y = result_set[, "fitted"], x = result_set[, "Date"], col = "red")
  text(y = 1.05, x = result_set[, "Date"][nrow(result_set)/2], 
       labels = paste0("Accuracy=", accuracy), col = "blue")
  grid()
  
}

KS <- function(estm, outc){
  index <- order(estm, decreasing = TRUE)
  estm <- estm[index]
  outc <- outc[index]
  N <- length(outc)
  cdf1 <- rep(0, N+1)
  cdf0 <- rep(0, N+1)
  D <- sum(outc == 1)
  for(i in 1:N){
    cdf0[i+1] <- cdf0[i]
    cdf1[i+1] <- cdf1[i]
    if(outc[i] == 0){
      cdf0[i+1] = cdf0[i+1] + 1.0/(N-D)
    }else{
      cdf1[i+1] = cdf1[i+1] + 1.0/D
    }
  }
  ks <- max(abs(cdf1-cdf0))
  plot(cdf0, type="l", main = paste0("CDF of Events(1) and non-events(0), KS = ", round(ks, 4)),
       xlab = "Index Ordering", ylab = "CDF")
  lines(cdf1, col=2)
  legend("topleft", bty = 'n', legend = c("CDF of 0", "CDF of 1"),
         col = c("black", "red"), lty = 1, lwd =2)
  return(ks)
}



