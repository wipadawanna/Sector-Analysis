rm(list = ls())
source("Performance_Analysis.R")
library(stats)
library(xts)
library(quantmod)
library(car)
library(glmnet)  ## Adding penalty to logistic
library(e1071)

col_ind <- function(x, ind){
  which(colnames(x) == ind)
}

#####################################################################################
load("logistic_xts.rdata")
#####################################################################################

N <- nrow(logistic_xts)
N_test <- 0.8*N
cost <- 0.8
train_set <- logistic_xts[1:N_test, ]
test_set <- logistic_xts[(N_test+1):N, ]
svm_radial <- svm(beat_mkt ~ ., data = train_set,
    kernel = "radial", cost = cost)

svm_predtrain <- predict(svm_radial, train_set)
train_prediction <- as.numeric(svm_predtrain > 0.5)
train_accuracy <- sum(as.numeric(train_prediction == train_set$beat_mkt))/N_test

svm_predtest <- predict(svm_radial, test_set)
test_prediction <- as.numeric(svm_predtest > 0.5)
test_accuracy <- sum(as.numeric(test_prediction == test_set$beat_mkt))/(N-N_test)
test_accuracy

png(filename = "Graphs/IYW_SVM_rolling_train_set.png",
    width = 7, height = 5, units = "in", res = 350)

plot(y = train_set$beat_mkt, x = as.Date(index(train_set)), 
     typ = "l", ylim = c(min(0, svm_predtrain), max(1, svm_predtrain) + 0.1), 
     xlab = "Date", ylab = "Probability", main = "SVM Prediction on training set(80/20)")
grid()
points(y = svm_predtrain,  x = as.Date(index(train_set)), col = "red")
text(x = as.Date(index(train_set)[N_test/3]), 
     y = max(1, svm_predtrain) + 0.1,
     labels = paste0("Train accuracy = ", round(train_accuracy, 5),
                     " Test accuracy = ", round(test_accuracy, 5)), 
     col = "blue")
lines(y = rep(0.5, N_test), x = as.Date(index(train_set)), col = "green")

dev.off()

###########################################################################

#find optimal parameters in a specified range
tune_out <- tune.svm(beat_mkt ~ ., data = logistic_xts,
                     gamma=10^(-3:3),
                     cost=c(0.01,0.1,1,10,100,1000),
                     kernel="radial")
best_C <- tune_out$best.parameters$cost
best_gamma <- tune_out$best.parameters$gamma

svm_model_kernel <- function(input_xts, train_windows, predict_window, 
                             kernel = "radial", cost = 1, gamma = 0.1){
  N <- nrow(input_xts)
  result_set <- NULL
  
  for(i in 1:(N - train_windows - predict_window + 1)){
    last_index <- (i-1+train_windows)
    train_set <- input_xts[i:last_index, ]
    test_set <- input_xts[(last_index+1):(last_index+predict_window), ]
    
    svm_model <- svm(beat_mkt ~ ., data = train_set,
                      kernel = kernel, cost = cost, gamma = gamma)
    svm_predtest <- predict(svm_model, test_set)
    test_value <- as.numeric(svm_predtest)
    
    tmp <- data.frame(as.data.frame(index(test_set)), 
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
    "predict_window" = predict_window,
    "kernel" = kernel,
    "cost" = cost,
    "gamma" = gamma
  ))
}


# train_length <- seq(from=84, to=120, by = 12)
# predict_length <- c(1:12)
# cost_list <- c(0.01,0.1,1,10,100,1000)
# gamma_list <- 10^(-3:3)
# max_accuracy <- 0.0
# params_list <- NULL
# for(train_windows in train_length){
#   for(predict_window in predict_length){
#     for(C in cost_list){
#       for(gamma in gamma_list){
#         results <- svm_model_kernel(logistic_xts, train_windows, predict_window, 
#                                     kernel = "radial", cost = C, gamma = gamma)
#         
#         if(results$accuracy > max_accuracy){
#           max_accuracy <- results$accuracy
#           params_list <- results
#         }
#       }
#     }
#   }
# }
# train_windows <- params_list$train_windows
# predict_window <- params_list$predict_window
# result_set <- params_list$result_set
# accuracy <- params_list$accuracy
# gamma <- params_list$gamma
# cost <- params_list$cost

####### Best Model based on accuracy
####### train_windows <- 96
####### predict_window <- 1
####### accuracy <- 62.26%
####### gamma <- 0.1
####### cost <- 10

train_windows <- 96
predict_window <- 1
gamma <- 0.1
cost <- 10

optimal_svm <- svm_model_kernel(logistic_xts, train_windows, predict_window, 
                             kernel = "radial", cost, gamma)

result_set_svm <- optimal_svm$result_set
accuracy <- optimal_svm$accuracy

data_length <- nrow(logistic_xts) - train_windows +1

png(filename = "Graphs/IYW_SVM_rolling_penalize.png",
    width = 7, height = 5, units = "in", res = 350)

plot(y = logistic_xts[, "beat_mkt"][train_windows:nrow(logistic_xts)], 
     x = index(logistic_xts)[train_windows:nrow(logistic_xts)], 
     typ = "l", pch = 19, main = paste("SVM Rolling", train_windows, "months for next", 
                                       predict_window, "months, g = ", gamma, "C = ", cost),
     ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
grid()
lines(y = rep(0.5, data_length), x = index(logistic_xts)[train_windows:nrow(logistic_xts)], col = "green")
points(y = result_set_svm[, "fitted"], x = result_set_svm[, "Date"], col = "red")
text(y = 1.05, x = result_set_svm[, "Date"][nrow(result_set_svm)/2], 
     labels = paste0("Accuracy=", accuracy), col = "blue")
dev.off()