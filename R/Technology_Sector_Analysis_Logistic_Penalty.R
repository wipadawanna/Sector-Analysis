rm(list = ls())
source("Performance_Analysis.R")
library(stats)
library(xts)
library(quantmod)
library(car)
library(glmnet)  ## Adding penalty to logistic


col_ind <- function(x, ind){
  which(colnames(x) == ind)
}

#####################################################################################
load("logistic_xts.rdata")
#####################################################################################

general_logistic_reg <- function(input_xts, train_windows, predict_window){
  N <- nrow(input_xts)
  result_set <- NULL
  
  for(i in 1:(N - train_windows - predict_window + 1)){
    last_index <- (i-1+train_windows)
    train_set <- input_xts[i:last_index, ]
    test_set <- input_xts[(last_index+1):(last_index+predict_window), ]
    
    train_model <- glm(beat_mkt~., family = "binomial", data = train_set)
    train_model_sum <- summary(train_model)
    
    test_Ax <- predict(train_model, test_set)
    test_prob <- exp(test_Ax)/(1+exp(test_Ax))
    test_value <- as.numeric(test_prob)
    
    tmp <- data.frame(as.data.frame(as.Date(index(test_set))), 
                      as.data.frame(test_value), 
                      as.data.frame(coredata(test_set[, "beat_mkt"])))
    colnames(tmp) <- c("Date", "fitted", "actual")
    result_set <- rbind(result_set, tmp)
  }
  predicted_values <- as.numeric(result_set[, "fitted"] > 0.5)
  accuracy <- sum(!xor(result_set[, "actual"], predicted_values))/length(predicted_values)
  
  return(list(
    "accuracy" = accuracy,
    "result_set" = result_set,
    "train_windows" = train_windows,
    "predict_window" = predict_window
  ))
}

# train_length <- seq(from=60, to=120, by = 12)
# predict_length <- c(1:12)
# max_accuracy <- 0.0
# params_list <- NULL
# for(train_windows in train_length){
#   for(predict_window in predict_length){
#     results <- general_logistic_reg(logistic_xts, train_windows, predict_window)
#     if(results$accuracy > max_accuracy){
#       max_accuracy <- results$accuracy
#       params_list <- results
#     }
#   }
# }
# 
# train_windows <- params_list$train_windows
# predict_window <- params_list$predict_window
# result_set <- params_list$result_set
# accuracy <- params_list$accuracy

####### Best Model based on accuracy
####### train_windows <- 120
####### predict_window <- 2
####### accuracy <- 61.7%

train_windows <- 120
predict_window <- 2
optimal_params <- general_logistic_reg(logistic_xts, 
                                      train_windows = train_windows, 
                                      predict_window = predict_window)
accuracy <- optimal_params$accuracy
result_set <- optimal_params$result_set

data_length <- nrow(logistic_xts) - train_windows +1

png(filename = "Graphs/IYW_logistic_reg_rolling.png",
    width = 7, height = 5, units = "in", res = 350)
plot(y = logistic_xts[, "beat_mkt"][train_windows:nrow(logistic_xts)], 
     x = as.Date(index(logistic_xts)[train_windows:nrow(logistic_xts)]), 
     typ = "l", pch = 19, 
     main = paste("Logistic Reg: Rolling", train_windows, "months for next", predict_window, "months"),
     ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
grid()
lines(y = rep(0.5, data_length), 
      x = as.Date(index(logistic_xts)[train_windows:nrow(logistic_xts)]), 
                  col = "green")
points(y = result_set[, "fitted"], x = as.Date(result_set[, "Date"]), col = "red")
text(y = 1.05, x = result_set[, "Date"][nrow(result_set)/2], 
     labels = paste0("Accuracy=", accuracy), col = "blue")
dev.off()
######################################################################

elastic_logistic_reg <- function(input_xts, train_windows, predict_window, alpha){
  N <- nrow(input_xts)
  result_set_elastic <- NULL
  for(i in 1:(N - train_windows - predict_window + 1)){
    last_index <- (i-1+train_windows)
    train_set <- logistic_xts[i:last_index, ]
    test_set <- logistic_xts[(last_index+1):(last_index+predict_window), ]
    
    train_model_robust <- glmnet(x = coredata(train_set[,-c(col_ind(train_set, "beat_mkt"))]),
                                 y = coredata(train_set[,"beat_mkt"]), 
                                 family = "binomial",
                                 intercept = FALSE,
                                 alpha = alpha)
    #plot(train_model_robust, xvar='lambda')
    train_model_robust_cv <- cv.glmnet(x = coredata(train_set[,-c(col_ind(train_set, "beat_mkt"))]), 
                                       y = coredata(train_set[,"beat_mkt"]))
    #plot(train_model_robust_cv)
    lambdamin <- train_model_robust_cv$lambda.min
    lambda1se <- train_model_robust_cv$lambda.1se
    
    robust_coef <- as.data.frame(as.matrix(coef(train_model_robust, 
                                                s = lambdamin)))
    
    test_Ax <- cbind("(Intercept)" = rep(1, predict_window), 
                     test_set[, -c(col_ind(test_set, "beat_mkt"))])
    test_Ax <- as.matrix(test_Ax)%*%as.matrix(robust_coef)
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
  
  return(list(
    "accuracy" = accuracy,
    "result_set" = result_set_elastic,
    "train_windows" = train_windows,
    "predict_window" = predict_window,
    "alpha" = alpha
  ))
}
  
# train_length <- seq(from=84, to=120, by = 12)
# predict_length <- c(1:6)
# alpha_list <- seq(0, 1, by = 0.2)
# max_accuracy <- 0.0
# params_list <- NULL
# for(train_windows in train_length){
#   for(predict_window in predict_length){
#     for(alpha in alpha_list){
#       results <- elastic_logistic_reg(logistic_xts, train_windows, predict_window, alpha = alpha)
#       if(results$accuracy > max_accuracy){
#         max_accuracy <- results$accuracy
#         params_list <- results
#       }
#     }
#   }
# }
# train_windows <- params_list$train_windows
# predict_window <- params_list$predict_window
# result_set_elastic <- params_list$result_set
# alpha <- params_list$alpha
# accuracy <- params_list$accuracy

####### Best Model based on accuracy
####### train_windows <- 96
####### predict_window <- 3
####### accuracy <- 59.615%
####### alpha <- 0.8

train_windows <- 96
predict_window <- 3
alpha <- 0.8

optimal_elastic <- elastic_logistic_reg(logistic_xts, train_windows, predict_window, alpha)
accuracy <- optimal_elastic$accuracy
result_set_elastic <- optimal_elastic$result_set

png(filename = "Graphs/IYW_logistic_elastic_rolling.png",
    width = 7, height = 5, units = "in", res = 350)

data_length <- nrow(logistic_xts) - train_windows + 1
plot(y = logistic_xts[, "beat_mkt"][train_windows:nrow(logistic_xts)], 
     x = index(logistic_xts)[train_windows:nrow(logistic_xts)], 
     typ = "l", pch = 19, main = paste("Rolling", train_windows, 
                                       "months:robust for next", 
                                       predict_window, "months with alpha = ", alpha),
     ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
grid()
lines(y = rep(0.5, data_length), 
      x = index(logistic_xts)[train_windows:nrow(logistic_xts)], 
      col = "green")
points(y = result_set_elastic[, "fitted"], x = result_set_elastic[, "Date"], col = "red")
text(y = 1.05, x = result_set_elastic[, "Date"][nrow(result_set_elastic)/2], 
     labels = paste0("Accuracy=", accuracy), col = "blue")

dev.off()