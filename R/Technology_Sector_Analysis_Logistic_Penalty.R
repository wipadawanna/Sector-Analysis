rm(list = ls())
library(stats)
library(xts)
library(quantmod)
library(car)
library(glmnet)  ## Adding penalty to logistic

col_ind <- function(x, ind){
  which(colnames(x) == ind)
}

load("dat_xts.rdata")
load("excess_xts.rdata")
load("merge_xts.rdata")
load("merge_xts_excess.rdata")

#####################################################################################
load("logistic_xts.rdata")
#####################################################################################

train_windows <- 120
predict_window <- 6
N <- nrow(logistic_xts)
result_set <- NULL

for(i in 1:(N - train_windows - predict_window + 1)){
  last_index <- (i-1+train_windows)
  train_set <- logistic_xts[i:last_index, ]
  test_set <- logistic_xts[(last_index+1):(last_index+predict_window), ]
  
  train_model <- glm(beat_mkt~.-1, family = "binomial", data = train_set)
  train_model_sum <- summary(train_model)
  
  test_Ax <- predict(train_model, test_set)
  test_prob <- exp(test_Ax)/(1+exp(test_Ax))
  test_value <- as.numeric(test_prob)
  
  tmp <- data.frame(as.data.frame(index(test_set)), 
                    as.data.frame(test_value), 
                    as.data.frame(coredata(test_set[, "beat_mkt"])))
  colnames(tmp) <- c("Date", "fitted", "actual")
  result_set <- rbind(result_set, tmp)
}

predicted_values <- as.numeric(result_set[, "fitted"] > 0.5)
accuracy <- sum(!xor(result_set[, "actual"], predicted_values))/length(predicted_values)
accuracy

data_length <- nrow(logistic_xts) - train_windows +1

plot(y = logistic_xts[, "beat_mkt"][train_windows:nrow(logistic_xts)], 
     x = index(logistic_xts)[train_windows:nrow(logistic_xts)], 
     typ = "l", pch = 19, main = paste("Rolling Logistic Regression for next", predict_window, "months"),
     ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
lines(y = rep(0.5, data_length), x = index(logistic_xts)[train_windows:nrow(logistic_xts)], col = "green")
points(y = result_set[, "fitted"], x = result_set[, "Date"], col = "red")
text(y = 1.05, x = result_set[, "Date"][nrow(result_set)/2], 
     labels = paste0("Accuracy=", accuracy), col = "blue")

######################################################################
train_windows <- 120
predict_window <- 3
N <- nrow(logistic_xts)
result_set_elastic <- NULL

for(i in 1:(N - train_windows - predict_window + 1)){
  last_index <- (i-1+train_windows)
  train_set <- logistic_xts[i:last_index, ]
  test_set <- logistic_xts[(last_index+1):(last_index+predict_window), ]
  
  train_model_robust <- glmnet(x = coredata(train_set[,-c(col_ind(train_set, "beat_mkt"))]),
                               y = coredata(train_set[,"beat_mkt"]), 
                               family = "binomial",
                               intercept = FALSE,
                               alpha = 0.5)
  #plot(train_model_robust, xvar='lambda')
  train_model_robust_cv <- cv.glmnet(x = coredata(train_set[,-c(col_ind(train_set, "beat_mkt"))]), 
                                     y = coredata(train_set[,"beat_mkt"]))
  #plot(train_model_robust_cv)
  lambdamin <- train_model_robust_cv$lambda.min
  lambda1se <- train_model_robust_cv$lambda.1se
  
  robust_coef <- as.data.frame(as.matrix(coef(train_model_robust, 
       s = lambdamin)))[-1, ] ## Drop the intercept
  
  test_Ax <- test_set[, -c(col_ind(test_set, "beat_mkt"))]%*%robust_coef
  test_prob <- exp(test_Ax)/(1+exp(test_Ax))
  test_value <- as.numeric(test_prob)
  
  tmp <- data.frame(as.data.frame(index(test_set)), 
                    as.data.frame(test_value), 
                    as.data.frame(coredata(test_set[, "beat_mkt"])))
  colnames(tmp) <- c("Date", "fitted", "actual")
  result_set_elastic <- rbind(result_set_elastic, tmp)
}

predicted_values <- as.numeric(result_set_elastic[, "fitted"] > 0.5)
accuracy <- sum(!xor(result_set_elastic[, "actual"], predicted_values))/length(predicted_values)
accuracy

data_length <- nrow(logistic_xts) - train_windows +1
plot(y = logistic_xts[, "beat_mkt"][train_windows:nrow(logistic_xts)], 
     x = index(logistic_xts)[train_windows:nrow(logistic_xts)], 
     typ = "l", pch = 19, main = paste("Robust Logistic Regression for next", predict_window, "months"),
     ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
lines(y = rep(0.5, data_length), x = index(logistic_xts)[train_windows:nrow(logistic_xts)], col = "green")
points(y = result_set_elastic[, "fitted"], x = result_set_elastic[, "Date"], col = "red")
text(y = 1.05, x = result_set_elastic[, "Date"][nrow(result_set_elastic)/2], 
     labels = paste0("Accuracy=", accuracy), col = "blue")