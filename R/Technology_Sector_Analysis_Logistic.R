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
load("merge_xts.rdata")
load("merge_xts_excess.rdata")


#used_data_xts <- excess_xts
used_data_xts <- merge_xts_excess

N <- nrow(used_data_xts)
beat_mkt <- as.numeric(used_data_xts$excess_ret > 0)
logistic_xts <- used_data_xts[, -c(col_ind(used_data_xts, "excess_ret"))]
tmpcol <- colnames(logistic_xts)
logistic_xts <- cbind(beat_mkt, logistic_xts)
colnames(logistic_xts) <- c("beat_mkt", tmpcol)

#********
table_writing <- data.frame("Date" = as.Date(index(logistic_xts)), coredata(logistic_xts))
write.csv(table_writing, "input_data_tech.csv", row.names = F)

simple_model <- glm(beat_mkt~.-1, family = "binomial", data = logistic_xts)
simple_model_sum <- summary(simple_model)
simple_model_sum

prediction <- as.numeric(simple_model$fitted.values > 0.5)
acc <- sum(prediction == logistic_xts$beat_mkt)/N

plot(y = logistic_xts$beat_mkt, x = index(logistic_xts), type = "l", 
     main = "Simple Model: IYW Beating SP500", ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
lines(y = simple_model$fitted.values, x = index(logistic_xts), col = "red")
text(y = 1.05, x = index(logistic_xts)[N/2], labels = paste0("Accuracy=", acc), col = "blue")

#############################################################

select_significant_attributes <- function(model, threshold = 0.1){
  model_coef <- coef(simple_model_sum)
  p_value <- model_coef[,"Pr(>|z|)"]
  significant_attr <- names(p_value)[(p_value <= threshold)]
  return(significant_attr)
}

signif_attr_simple <- select_significant_attributes(simple_model_sum, threshold = 0.1)

formula_string <- ""
for(vars in signif_attr_simple){
  formula_string <- paste0(formula_string, "+", vars)
}
formula_string <- substring(formula_string, 2)
formula_string <- paste0("beat_mkt~", formula_string, "-1")

reduced_model <- glm(formula_string, family = "binomial", data = logistic_xts)
reduced_model_sum <- summary(reduced_model)
reduced_model_sum

prediction <- as.numeric(reduced_model$fitted.values > 0.5)
acc_reduced <- sum(prediction == logistic_xts$beat_mkt)/N

plot(y = logistic_xts$beat_mkt, x = index(logistic_xts), type = "l", 
     main = "Reduced Model: IYW Beating SP500", ylab = "Probability", xlab = "Date", ylim = c(0, 1.1))
lines(y = reduced_model$fitted.values, x = index(logistic_xts), col = "red")
text(y = 1.05, x = index(logistic_xts)[N/2], labels = paste0("Accuracy=", acc_reduced), col = "blue")

conf_level <- function(alpha, model, df_input, attr_list, N = 100000){
  x_input <- df_input[, attr_list]
  model_coef <- coef(model)[attr_list,"Estimate"]
  model_sd <- coef(model)[attr_list,"Std. Error"]
  N_mean <- sum(x_input*model_coef)
  N_sd <- sqrt(sum(x_input^2*model_sd^2))
  
  lowerq <- (1-alpha)/2.0
  upperq <- lowerq+alpha
  
  lowerN <- qnorm(lowerq, mean = N_mean, sd = N_sd)
  upperN <- qnorm(upperq, mean = N_mean, sd = N_sd)
  
  expected_prob <- exp(N_mean)/(1+exp(N_mean))
  lower_prob <- exp(lowerN)/(1+exp(lowerN))
  upper_prob <- exp(upperN)/(1+exp(upperN))
  
  rand <- rnorm(N, mean = N_mean, sd = N_sd)
  beating_prob <- exp(rand)/(1+exp(rand))
  
  return(list(conf_level = c(expected_prob, lower_prob, upper_prob), 
              prob = beating_prob))
}

#############################################################
## Test on data
textIndex <- nrow(logistic_xts)-30
testdata <- logistic_xts[textIndex, ]
return_list <- conf_level(alpha = 0.95, model = reduced_model_sum, 
                    df_input = testdata, 
                    attr_list = rownames(coef(reduced_model_sum)))
hist(return_list$prob, breaks = 100, 
     freq = TRUE, col = "yellow", xlab = "Probability", main = paste0("Histogram of Beating Probability \n
     Expected = ", round(return_list$conf_level[1], 4)))
return_list$conf_level

reduced_model$fitted.values[textIndex]

##############################################################

