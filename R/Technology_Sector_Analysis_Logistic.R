rm(list = ls())
library(stats)
library(xts)
library(quantmod)
library(car)

col_ind <- function(x, ind){
  which(colnames(x) == ind)
}

load("dat_xts.rdata")
load("excess_xts.rdata")

N <- nrow(excess_xts)
beat_mkt <- as.numeric(excess_xts$excess_ret > 0)
logistic_xts <- excess_xts[, -c(col_ind(excess_xts, "excess_ret"))]
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


reduced_model <- glm(beat_mkt~SP500+CSENT+XOI.Index+CSUSHPINSA+GDP-1, 
                     family = "binomial", data = logistic_xts)
reduced_model_sum <- summary(reduced_model)
reduced_model_sum

prediction <- as.numeric(reduced_model$fitted.values > 0.5)
acc_reduced <- sum(prediction == logistic_xts$beat_mkt)/N

plot(y = logistic_xts$beat_mkt, x = index(logistic_xts), type = "l", 
     main = "Reduced Model: IYW Beating SP500", ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
lines(y = reduced_model$fitted.values, x = index(logistic_xts), col = "red")
text(y = 1.05, x = index(logistic_xts)[N/2], labels = paste0("Accuracy=", acc_reduced), col = "blue")

