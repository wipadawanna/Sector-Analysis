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

#used_data_xts <- excess_xts
used_data_xts <- merge_xts_excess

N <- nrow(used_data_xts)
beat_mkt <- as.numeric(used_data_xts$excess_ret > 0)
logistic_xts <- used_data_xts[, -c(col_ind(used_data_xts, "excess_ret"))]
tmpcol <- colnames(logistic_xts)
logistic_xts <- cbind(logistic_xts, beat_mkt)
colnames(logistic_xts) <- c(tmpcol, "beat_mkt")

simple_model <- glm(beat_mkt~.-1, family = "binomial", data = logistic_xts)
simple_model_sum <- summary(simple_model)
simple_model_sum

prediction <- as.numeric(simple_model$fitted.values > 0.5)
acc <- sum(prediction == logistic_xts$beat_mkt)/N

plot(y = logistic_xts$beat_mkt, x = index(logistic_xts), type = "l", 
     main = "Simple Model: IYW Beating SP500", ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
lines(y = simple_model$fitted.values, x = index(logistic_xts), col = "red")
text(y = 1.05, x = index(logistic_xts)[N/2], labels = paste0("Accuracy=", acc), col = "blue")

#####################################################################################

train_windows <- 60
predict_window <- 3
N <- nrow(logistic_xts)

result_set <- NULL

for(i in 1:(N - train_windows - predict_window + 1)){
  last_index <- (i-1+train_windows)
  train_set <- logistic_xts[i:last_index, ]
  test_set <- logistic_xts[last_index:(last_index+2), ]
  
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

plot(y = result_set[, "actual"], x = result_set[, "Date"], 
     typ = "l", main = paste("Rolling Logistic Regression for next", predict_window, "months"),
     ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
#lines(y = predicted_values, x = result_set[, "Date"], col = "red")
lines(y = result_set[, "fitted"], x = result_set[, "Date"], col = "red")
text(y = 1.05, x = result_set[, "Date"][nrow(result_set)/2], labels = paste0("Accuracy=", accuracy), col = "blue")
