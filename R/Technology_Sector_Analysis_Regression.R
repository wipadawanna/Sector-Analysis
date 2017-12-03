rm(list = ls())
library(stats)
library(xts)
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
dat_xts <- xts(dat_data, order.by = dat_timestamp)

plot(y = dat_xts$benchmark, x = dat_timestamp, ylim = c(-0.15, 0.20),
     type = "l", ylab = "return", xlab = "Date", main = "IYW vs SP500")
lines(y = dat_xts$SP500, x = dat_timestamp, col = "red")
text(y = 0.15, x = dat_timestamp[length(dat_timestamp)-10], 
     labels = paste0("Cor = ", cor(dat_xts$benchmark, dat_xts$SP500)), col = "blue")
legend("top", bty = 'n', legend = c("IYW", "SP500"), col=c("black", "red"), lty = 1, lwd = 2)

##################################################################################
########################## Linear Regression #####################################
########################## Simple IYW Return #####################################

simple_model <- lm(benchmark~., data = dat_xts)
simple_res <- summary(simple_model)
simple_res

simple_model_reduce <- lm(benchmark~GDP+PCE+VIXCLS+XOI.Index+XAU.Curncy+PAYEMS+OAS, data = dat_xts)
simple_res_reduce <- summary(simple_model_reduce)
simple_res_reduce

plot_lm_actual_fitted <- function(fitted, actual, timestamp, r2, plotname){
  plot(y = fitted, x = timestamp, 
       type = "l", ylim = c(min(c(fitted,actual)), max(c(fitted,actual))+0.03), 
       main = plotname, ylab = "return", xlab = "Date")
  lines(y = actual, x = timestamp, col = "red")
  text(y = max(c(fitted,actual)), x = timestamp[length(timestamp)-10], 
       labels = paste0("R2 = ", r2), col = "blue")
  legend("top", bty = 'n', legend = c("Fitted", "Realized"), 
         col=c("black", "red"), lty = 1, lwd = 2)
}

plot_lm_actual_fitted(simple_model$fitted.values, 
                      dat_xts$benchmark, 
                      dat_timestamp, 
                      simple_res$r.squared,
                      "IYW - Full Model Realized vs Fitted")

plot_lm_actual_fitted(simple_model_reduce$fitted.values, 
                      dat_xts$benchmark, 
                      dat_timestamp, 
                      simple_res_reduce$r.squared,
                      "IYW - Reduced Model Realized vs Fitted")

##################################################################################
########################## Linear Regression #####################################
########################## Excess IYW Return #####################################

excess_return <- dat_xts$benchmark - dat_xts$SP500
econ_data <- dat_xts[, -c(col_ind(dat_xts, "SP500"), col_ind(dat_xts, "benchmark"))]
excess_xts <- cbind(econ_data, excess_return)
colnames(excess_xts) <- c(colnames(econ_data), "excess_ret")
excess_xts <- xts(excess_xts, order.by = dat_timestamp)

excess_ret_model <- lm(excess_ret ~. , data = excess_xts)
excess_res <- summary(excess_ret_model)
excess_res

excess_ret_model_reduce <- lm(excess_ret~UNRATE+VIXCLS+XOI.Index+CSENT+OAS, data = excess_xts)
excess_reduce_res <- summary(excess_ret_model_reduce)
excess_reduce_res

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
  #tmp <- xts(tmp[, -1], order.by = as.Date(tmp$DATE, format = "%Y-%m-%d"), frequency = 12) 
  colnames(tmp) <- c(file)
  tmp <- to.monthly(tmp, indexAt = 'lastof', OHLC = F)
  assign(file, tmp)
  merge_xts <- merge.xts(merge_xts, get(file), join='inner')
  merge_xts_excess <- merge.xts(merge_xts_excess, get(file), join='inner')
}

enhance_model <- lm(benchmark~., data = merge_xts)
enhance_res <- summary(enhance_model)
enhance_res

enhance_model_reduce <- lm(benchmark~PPI_Semiconductor+GDP+PCE+VIXCLS+XOI.Index+XAU.Curncy+PAYEMS+OAS
                       , data = merge_xts)
enhance_reduce_res <- summary(enhance_model_reduce)
enhance_reduce_res

enhance_excess_model <- lm(excess_ret ~. , data = merge_xts_excess)
enhance_excess_res <- summary(enhance_excess_model)
enhance_excess_res

enhance_excess_model_reduce <- lm(excess_ret~PPI_SOFTWARE+VIXCLS+XOI.Index+CSENT+OAS
                                     , data = merge_xts_excess)
enhance_excess_reduce_res <- summary(enhance_excess_model_reduce)
enhance_excess_reduce_res

plot_lm_actual_fitted(enhance_excess_model$fitted.values, 
                      excess_return, 
                      dat_timestamp, 
                      enhance_excess_res$r.squared,
                      "Excess IYW - Full Model w/ PPI Realized vs Fitted")

plot_lm_actual_fitted(enhance_excess_model_reduce$fitted.values, 
                      excess_return, 
                      dat_timestamp, 
                      enhance_excess_reduce_res$r.squared,
                      "Excess IYW - Reduced Model w/ PPI Realized vs Fitted")
