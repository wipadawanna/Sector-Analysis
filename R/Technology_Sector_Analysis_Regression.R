rm(list = ls())
source("Performance_Analysis.R")
library(stats)
library(xts)
library(glmnet)
library(quantmod)
library(car)

col_ind <- function(x, ind){
  which(colnames(x) == ind)
}

##################################################################################
########################## Read Dataset ##########################################

dir_path <- paste0(getwd(), '/x_raw_y_tech.csv')
raw_dat <- read.csv(dir_path, header = T, as.is = T)
dat_timestamp <- as.Date(raw_dat$Date, format = "%m/%d/%Y")
dat_data <- raw_dat[, -1]
dat_colnames <- colnames(dat_data)

############### Convert Data  ##############
simple_ret_columns <- c("benchmark", "SP500", "CSUSHPINSA", 
                        "PCE", "VIXCLS","XOI.Index", "XAU.Curncy",
                        "CSENT", "PAYEMS")
quarterly_ret_columns <- c("GDP")
percent_columns <- c("DGS10", "RECPROUSM156N", "TEDRATE", "FEDFUNDS", "UNRATE", "YIELD_SLOPE", "OAS")
N <- nrow(dat_data)
for(col in simple_ret_columns){
  simret <- dat_data[, col][-1]/dat_data[, col][-N]-1
  dat_data[, col] <- c(NA, simret)
}
for(col in quarterly_ret_columns){
  Nbegin <- N-2
  qret <- dat_data[, col][-c(1:3)]/dat_data[, col][-c(Nbegin:N)]-1
  dat_data[, col] <- c(rep(NA, 3), qret)
}
for(col in percent_columns){
  dat_data[, col] <- dat_data[, col]/100.0
}

dat_timestamp <- dat_timestamp[-c(1:3)]
dat_xts <- xts(dat_data[-c(1:3),], order.by = dat_timestamp)

#****************************************
save(dat_xts, file="dat_xts.rdata")

############################################
png(filename = "Graphs/IYW_vs_SP500.png",
    width = 7, height = 5, units = "in", res = 350)
N <- length(index(dat_xts))
plot(y = dat_xts$benchmark, x = as.Date(index(dat_xts)), xaxt="n",
     type = "l", ylab = "return", main = "IYW vs SP500", xlab = "", cex.axis = 0.8)
axis(1, at=as.Date(index(dat_xts))[seq(2, N, by = 10)], 
     labels=as.Date(index(dat_xts))[seq(2, N, by = 10)], las = 2, cex.axis = 0.8)
grid()
lines(y = dat_xts$SP500, x = as.Date(index(dat_xts)), col = "red")
text(y = -0.2, x = as.Date("2010-01-01"), 
     labels = paste0("Cor = ", cor(dat_xts$benchmark, dat_xts$SP500)), col = "blue")
legend("topright", bty = 'n', legend = c("IYW", "SP500"), 
       col=c("black", "red"), lty = 1, lwd = 2)
dev.off()
##################################################################################
########################## Linear Regression #####################################
########################## Simple IYW Return #####################################

simple_model <- lm(benchmark~., data = dat_xts)
simple_res <- summary(simple_model)
simple_res
vif(simple_model)

simple_model_reduce <- lm(benchmark~SP500+CSUSHPINSA+PCE+XOI.Index+PAYEMS+CSENT, 
                          data = dat_xts)
simple_res_reduce <- summary(simple_model_reduce)
simple_res_reduce
vif(simple_model_reduce)

png(filename = "Graphs/IYW_linear_reg_simple.png",
    width = 7, height = 5, units = "in", res = 350)
plot_lm_actual_fitted(simple_model$fitted.values, 
                      dat_xts$benchmark, 
                      dat_timestamp, 
                      simple_res$r.squared,
                      "IYW - Linear Regression: Realized vs Fitted")
dev.off()

plot_lm_actual_fitted(simple_model_reduce$fitted.values, 
                      dat_xts$benchmark, 
                      dat_timestamp, 
                      simple_res_reduce$r.squared,
                      "IYW - Reduced Model Realized vs Fitted")

##################################################################################
########################## Linear Regression #####################################
########################## Excess IYW Return #####################################

excess_return <- dat_xts$benchmark - dat_xts$SP500
plot(excess_return, main = "IYW Outperformed SP500(% Return)")

econ_data <- dat_xts[, -c(col_ind(dat_xts, "benchmark"))]
excess_xts <- cbind(econ_data, excess_return)
colnames(excess_xts) <- c(colnames(econ_data), "excess_ret")
excess_xts <- xts(excess_xts, order.by = dat_timestamp)

#****************************************
save(excess_xts, file="excess_xts.rdata")

excess_ret_model <- lm(excess_ret ~. , data = excess_xts)
excess_res <- summary(excess_ret_model)
excess_res
vif(excess_ret_model)

excess_ret_model_reduce <- lm(excess_ret
                              ~CSUSHPINSA+PCE+XOI.Index+CSENT+PAYEMS+SP500, data = excess_xts)
excess_reduce_res <- summary(excess_ret_model_reduce)
excess_reduce_res
vif(excess_ret_model_reduce)

plot_lm_actual_fitted(excess_ret_model_reduce$fitted.values, 
                      excess_return, 
                      dat_timestamp, 
                      excess_reduce_res$r.squared,
                      "Excess IYW - Reduced Model Realized vs Fitted")

##################################################################################
######################### SectorRelated Data(PPI) ################################
####################### Simple & Excess IYW Return ###############################

wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
files_dir <- paste0(parent, "/data/SectorRelated/")
files_name <- c("TELECOMEXPORT", "PPI_TELECOM", "PPI_SOFTWARE", "PPI_Semiconductor")
merge_xts <- dat_xts
merge_xts_excess <- excess_xts
for (file in files_name){
  assign(file, read.csv(paste0(files_dir, file, ".csv"), header = T, as.is = T))
  tmp <- get(file)
  tmp <- xts(tmp[-1, -1]/tmp[-nrow(tmp), -1]-1,
             order.by = as.Date(tmp$DATE[-1], format = "%Y-%m-%d"), frequency = 12)
  colnames(tmp) <- c(file)
  tmp <- to.monthly(tmp, indexAt = 'lastof', OHLC = F)
  assign(file, tmp)
  merge_xts <- merge.xts(merge_xts, get(file), join='inner')
  merge_xts_excess <- merge.xts(merge_xts_excess, get(file), join='inner')
}
logistic_bool <- as.numeric(merge_xts$benchmark > merge_xts$SP500)
logistic_xts <- cbind("beat_mkt" = logistic_bool, 
                      merge_xts[, -c(col_ind(merge_xts, "benchmark"))])

save(logistic_xts, file = "logistic_xts.rdata")

enhance_model <- lm(benchmark~., data = merge_xts)
enhance_res <- summary(enhance_model)
enhance_res

#############################################
vif_model <- vif(enhance_model)
rm_col <- NULL
for(colnm in names(vif_model)){
  if(vif_model[colnm] > 5){
    rm_col <- c(rm_col, col_ind(merge_xts, colnm))
  }
}
rm_col
merge_xts <- merge_xts[, -rm_col]
##############################################
save(merge_xts, file="merge_xts.rdata")

enhance_model_2 <- lm(benchmark~., data = merge_xts)
enhance_res_2 <- summary(enhance_model)
enhance_res_2

png(filename = "Graphs/IYW_linear_reg_withPPI.png",
    width = 7, height = 5, units = "in", res = 350)
plot_lm_actual_fitted(enhance_model_2$fitted.values, 
                      merge_xts$benchmark, 
                      dat_timestamp, 
                      enhance_res_2$r.squared,
                      "IYW - Regression Model w/ PPI: Realized vs Fitted")
dev.off()

######################## Rolling Windows #########################

general_linear_reg <- function(input_xts, train_windows, predict_window){
  N <- nrow(input_xts)
  result_set <- NULL
  
  for(i in 1:(N - train_windows - predict_window + 1)){
    last_index <- (i-1+train_windows)
    train_set <- input_xts[i:last_index, ]
    test_set <- input_xts[(last_index+1):(last_index+predict_window), ]
    
    train_model <- lm(benchmark~., data = train_set)
    train_model_sum <- summary(train_model)
    
    test_Ax <- predict(train_model, test_set)
    test_value <- as.numeric(test_Ax)
    
    tmp <- data.frame(as.data.frame(as.Date(index(test_set))), 
                      as.data.frame(test_value), 
                      as.data.frame(coredata(test_set[, "benchmark"])),
                      as.data.frame(coredata(test_set[, "SP500"])))
    colnames(tmp) <- c("Date", "fitted", "actual", "SP500")
    result_set <- rbind(result_set, tmp)
  }
  predicted_values <- as.numeric(result_set[, "fitted"] > result_set[, "SP500"])
  true_values <- as.numeric(result_set[, "actual"] > result_set[, "SP500"])
  accuracy <- sum(!xor(true_values, predicted_values))/length(predicted_values)
  r2 <- rsq(result_set[, "fitted"], result_set[, "actual"])
  
  return(list(
    "accuracy" = accuracy,
    "r2" = r2,
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
#     results <- general_linear_reg(merge_xts, train_windows, predict_window)
#     if(results$accuracy > max_accuracy){
#       max_accuracy <- results$accuracy
#       params_list <- results
#     }
#   }
# }

# train_windows <- params_list$train_windows
# predict_window <- params_list$predict_window
# result_set <- params_list$result_set
# accuracy <- params_list$accuracy
# r2 <- params_list$r2

####### Best Model based on accuracy
####### train_windows <- 120
####### predict_window <- 3
####### accuracy <- 62.916%
####### r2 <- 0.6612

train_windows <- 120
predict_window <- 3
optimal_params <- general_linear_reg(merge_xts, 
                                     train_windows = train_windows, 
                                     predict_window = predict_window)
accuracy <- optimal_params$accuracy
result_set <- optimal_params$result_set
r2 <- optimal_params$r2

data_length <- nrow(merge_xts) - train_windows +1

png(filename = "Graphs/IYW_linear_reg_rolling.png",
    width = 7, height = 5, units = "in", res = 350)
actual_result <- as.numeric(result_set$actual > result_set$SP500)
predict_result <- as.numeric(result_set$fitted > result_set$SP500)
plot(x = as.Date(result_set$Date), 
     y = actual_result,
     typ = "l", col = "black", 
     main = paste("Linear Reg: Prediction", train_windows, 
                         "months for next", predict_window, "months"),
     xlab = "Date", ylab = "Prediction", ylim = c(0, 1.1))
grid()
lines(y = rep(0.5, length(result_set$Date)),
      x = as.Date(result_set$Date),
      col = "blue")
points(x = as.Date(result_set$Date), 
       y = predict_result,
      col = ifelse((actual_result == predict_result), "green", "red"))
text(y = 1.1, x = result_set[, "Date"][nrow(result_set)/2],
     labels = paste0("Accuracy = ", accuracy), col = "blue")
dev.off()
######################################################################

elastic_linear_reg <- function(input_xts, train_windows, predict_window, alp){
  N <- nrow(input_xts)
  result_set_elastic <- NULL
  for(i in 1:(N - train_windows - predict_window + 1)){
    last_index <- (i-1+train_windows)
    train_set <- input_xts[i:last_index, ]
    test_set <- input_xts[(last_index+1):(last_index+predict_window), ]
    
    train_model_robust <- glmnet(x = coredata(train_set[,-c(col_ind(train_set, "benchmark"))]),
                                 y = coredata(train_set[,"benchmark"]), 
                                 family = "gaussian",
                                 intercept = FALSE,
                                 alpha = alp)
    #plot(train_model_robust, xvar='lambda')
    train_model_robust_cv <- cv.glmnet(x = coredata(train_set[,-c(col_ind(train_set, "benchmark"))]), 
                                       y = coredata(train_set[,"benchmark"]))
    #plot(train_model_robust_cv)
    lambdamin <- train_model_robust_cv$lambda.min
    lambda1se <- train_model_robust_cv$lambda.1se
    
    robust_coef <- as.data.frame(as.matrix(coef(train_model_robust, 
                                                s = lambdamin)))
    
    test_Ax <- cbind("(Intercept)" = rep(1, predict_window), 
                     test_set[, -c(col_ind(test_set, "benchmark"))])
    test_Ax <- as.matrix(test_Ax)%*%as.matrix(robust_coef)
    test_value <- as.numeric(test_Ax)
    
    tmp <- data.frame(as.data.frame(index(test_set)), 
                      as.data.frame(test_value), 
                      as.data.frame(coredata(test_set[, "benchmark"])),
                      as.data.frame(coredata(test_set[, "SP500"])))
    colnames(tmp) <- c("Date", "fitted", "actual", "SP500")
    result_set_elastic <- rbind(result_set_elastic, tmp)
  }
  
  predicted_values <- as.numeric(result_set_elastic[, "fitted"] > result_set_elastic[, "SP500"])
  actual_values <- as.numeric(result_set_elastic[, "actual"] > result_set_elastic[, "SP500"])
  accuracy <- sum(!xor(predicted_values, actual_values))/length(predicted_values)
  
  return(list(
    "accuracy" = accuracy,
    "result_set" = result_set_elastic,
    "train_windows" = train_windows,
    "predict_window" = predict_window,
    "alpha" = alp
  ))
}

train_length <- seq(from=84, to=120, by = 12)
predict_length <- c(1:6)
list_alpha <- seq(0, 1, by = 0.2)
max_accuracy <- 0.0
params_list <- NULL
for(train_windows in train_length){
  for(predict_window in predict_length){
    for(alpha in list_alpha){
      results <- elastic_linear_reg(merge_xts, train_windows, predict_window, alpha)
      if(results$accuracy > max_accuracy){
        max_accuracy <- results$accuracy
        params_list <- results
      }
    }
  }
}
train_windows <- params_list$train_windows
predict_window <- params_list$predict_window
result_set_elastic <- params_list$result_set
alpha <- params_list$alpha
accuracy <- params_list$accuracy

####### Best Model based on accuracy
####### train_windows <- 108
####### predict_window <- 1
####### accuracy <- 60.63%
####### alpha <- 0.8
####### lambdamin
#######

train_windows <- 108
predict_window <- 1
alpha <- 0.8
optimal_elastic <- elastic_linear_reg(merge_xts, train_windows, predict_window, 
                                        alpha)
accuracy <- optimal_elastic$accuracy
result_set_elastic <- optimal_elastic$result_set

png(filename = "Graphs/IYW_linear_elastic_rolling.png",
    width = 7, height = 5, units = "in", res = 350)
actual_result <- as.numeric(result_set_elastic$actual > result_set_elastic$SP500)
predict_result <- as.numeric(result_set_elastic$fitted > result_set_elastic$SP500)
plot(x = as.Date(result_set_elastic$Date), 
     y = actual_result,
     typ = "l", col = "black", 
     main = paste("Elastic Net Linear Reg: Prediction", train_windows, 
                  "months for next", predict_window, "months"),
     xlab = "Date", ylab = "Prediction", ylim = c(0, 1.1))
grid()
lines(y = rep(0.5, length(result_set_elastic$Date)),
      x = as.Date(result_set_elastic$Date),
      col = "blue")
points(x = as.Date(result_set_elastic$Date), 
       y = predict_result,
       col = ifelse((actual_result == predict_result), "green", "red"))
text(y = 1.1, x = result_set_elastic[, "Date"][nrow(result_set_elastic)/2],
     labels = paste0("Accuracy = ", accuracy), col = "blue")
dev.off()