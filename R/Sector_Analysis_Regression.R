rm(list = ls())
library(stats)

fin_path <- paste0(getwd(), '/x_raw_y_fin.csv')
fin.dat <- read.csv(fin_path, header = T, as.is = T)
fin.timestamp <- fin.dat$Date
fin.rawdat <- fin.dat[, -1]

demean <- function(x){
  return(x - mean(x))/sqrt(var(x))
}

fin.demean <- as.data.frame(apply(fin.rawdat, 2, demean)) 

fin.lm <- lm(beat_benchmark~., data = fin.demean)
summary(fin.lm)

model2 <- lm(beat_benchmark~VIXCLS + RECPROUSM156N + UNRATE, data = fin.demean)
summary(model2)



######################################
tec_path <- paste0(getwd(), '/x_raw_y_tech.csv')
tech.dat <- read.csv(tec_path, header = T, as.is = T)
tec.timestamp <- tech.dat$Date
tec.rawdat <- tech.dat[, -1]

tec.demean <- as.data.frame(apply(tec.rawdat, 2, demean)) 

tec.lm <- lm(beat_benchmark~., data = tec.demean)
summary(tec.lm)

model2 <- lm(beat_benchmark~DGS10 + UNRATE + VIXCLS + XAU.Curncy, data = tec.demean)
summary(model2)
