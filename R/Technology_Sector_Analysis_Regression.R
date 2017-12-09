rm(list = ls())
source("Performance_Analysis.R")
library(stats)
library(xts)
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

logistic_bool <- as.numeric(merge_xts_excess$excess_ret > 0)
logistic_xts <- cbind("beat_mkt" = logistic_bool, 
                                 merge_xts_excess[, -c(col_ind(merge_xts_excess, "excess_ret"))])
#**************************************
save(merge_xts, file="merge_xts.rdata")
save(merge_xts_excess, file="merge_xts_excess.rdata")
save(logistic_xts, file = "logistic_xts.rdata")

enhance_model <- lm(benchmark~., data = merge_xts)
enhance_res <- summary(enhance_model)
enhance_res
vif(enhance_model)

png(filename = "Graphs/IYW_linear_reg_withPPI.png",
    width = 7, height = 5, units = "in", res = 350)
plot_lm_actual_fitted(enhance_model$fitted.values, 
                      merge_xts$benchmark, 
                      dat_timestamp, 
                      enhance_res$r.squared,
                      "IYW - Regression Model w/ PPI: Realized vs Fitted")
dev.off()

enhance_model_reduce <- lm(benchmark
                           ~ CSUSHPINSA+FEDFUNDS+PCE+XOI.Index+CSENT+PAYEMS+SP500+TELECOMEXPORT+PPI_SOFTWARE
                       , data = merge_xts)
enhance_reduce_res <- summary(enhance_model_reduce)
enhance_reduce_res
vif(enhance_model_reduce)

enhance_excess_model <- lm(excess_ret ~. , data = merge_xts_excess)
enhance_excess_res <- summary(enhance_excess_model)
enhance_excess_res
vif(enhance_excess_model)

enhance_excess_model_reduce <- lm(excess_ret~CSUSHPINSA+PCE+XOI.Index+CSENT+PAYEMS+SP500
                                  +TELECOMEXPORT+PPI_SOFTWARE
                                     , data = merge_xts_excess)
enhance_excess_reduce_res <- summary(enhance_excess_model_reduce)
enhance_excess_reduce_res
vif(enhance_excess_model_reduce)

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
