### Forecasting Electricity price ###

library(ggplot2)
library(GGally)
library(tidyverse)
library(patchwork)
library(lubridate, warn.conflicts = FALSE)
library(dplyr)
library(corrplot)
install.packages("nlWaldTest")
install.packages("forecast")
library(lmtest)
library(dynlm)
library(orcutt)
library(sandwich)
library(car)
library(nlWaldTest)
library(forecast)
library(xts)

### Line of code which checks if the package is already installed
if(!require(nlWaldTest)) install.packages("nlWaldTest")

data <- read.table('ml_data.csv',header = T, sep=",")
summary(data)
head(data)

data.18.19 <- data[1:17520,]
summary(data.18.19)

data.18.19 <- data.18.19[,-1]

#tolgo le covariate meno significative
data.18.19<-data.18.19[,-c(2,3,4,5,6,7,8,9)]
data.18.19<-data.18.19[,-4]
summary(data.18.19)
#hydro, pv, self.consuption
data.18.19$geothermal=(data.18.19$geothermal10+data.18.19$geothermal9)/2
data.18.19$thermal=(data.18.19$thermal10+data.18.19$thermal9)/2
data.18.19$hydro=(data.18.19$hydro10+data.18.19$hydro9)/2
data.18.19$pv=(data.18.19$pv10+data.18.19$pv9)/2
data.18.19$self.consuption=(data.18.19$self.consumption9+data.18.19$self.consumption10)/2
data.18.19$wind=(data.18.19$wind10+data.18.19$wind9)/2
data.18.19<-data.18.19[,-c(5,6,7,8,9,10,11,12,13,14,15,16)]

#creo nuova variabile weekend
data.18.19$weekend<-ifelse(data.18.19$weekday==0|data.18.19$weekday==6,1,0)
attach(data.18.19)

#creo la covariata season categorica
#1=inverno, 2 =primavera 3=estate 4=autunno
season <- rep(0, length(month)) # Crea un vettore di zeri della stessa lunghezza di month
for (i in 1:length(month)) {
  if (month[i] %in% c(12, 1, 2)) {
    season[i] <- 1
  } else if (month[i] %in% c(3, 4, 5)) {
    season[i] <- 2
  } else if (month[i] %in% c(6, 7, 8)) {
    season[i] <- 3
  } else if (month[i] %in% c(9, 10, 11)) {
    season[i] <- 1
  }
}
data.18.19$season=season
#rimuovo month
data.18.19=data.18.19[,-4]

#grafico correlazione
library(corrplot)
corr<-cor(data.18.19)
x11()
corrplot(corr,method="number")
#troppe variabili correlate

#trasformo le covariate in factor
hour=as.factor(hour)
weekend=as.factor(weekend)
season=as.factor(season)


#modello dinamico
data.18.19 <- ts(data.18.19, start = c(2018, 1), end = c(2019,12), frequency = 17520)

gas_price<- ts(data.18.19[,7], start = c(2018, 1), end = c(2019,12), frequency = 17520)
dam <- ts(data.18.19[,1], start = c(2018, 1), end = c(2019,12), frequency = 17520)
Forecasted_load<- ts(data.18.19[,2], start = c(2018, 1), end = c(2019,12), frequency = 17520)
thermal<- ts(data.18.19[,9], start = c(2018, 1), end = c(2019,12), frequency = 17520)
gas_price <- window(gas_price,start=c(2018,1), end = c(2019,12))
dam <- window(dam,start=c(2018,1), end = c(2019,12))
Forecasted_load <- window(Forecasted_load,start=c(2018,1), end = c(2019,12))
thermal <- window(thermal,start=c(2018,1), end = c(2019,12))

fit1 <- dynlm(dam ~ Forecasted_load+gas_price+L(dam,1)+thermal)
summary(fit1)

fit2 <- dynlm(dam ~ Forecasted_load+gas_price+L(dam,1)+ L(dam, 2))
summary(fit2)

fit6<- dynlm(dam ~ Forecasted_load+gas_price+L(dam,1)+ L(dam, 2)+ L(dam, 3) +
               L(dam, 4) + L(dam, 5) + L(dam, 6))
summary(fit6)

fit12<- dynlm(dam ~ Forecasted_load+gas_price+L(dam,1)+ L(dam, 2)+ L(dam, 3) +
                L(dam, 4) + L(dam, 5) + L(dam, 6)+L(dam,7) + L(dam, 8)+ L(dam, 9) +
                L(dam, 10) + L(dam, 11) + L(dam, 12))
summary(fit12)


fit24<- dynlm(dam ~ Forecasted_load+gas_price+thermal+L(dam,1)+ L(dam, 2)+ L(dam, 3) +
                L(dam, 4) + L(dam, 5) + L(dam, 6)+L(dam,7) + L(dam, 8)+ L(dam, 9) +
                L(dam, 10) + L(dam, 11) + L(dam, 12)+L(dam, 13) + L(dam, 14)+ L(dam, 15) +
                L(dam, 16) + L(dam, 17) + L(dam, 18)+L(dam,19) + L(dam, 20)+ L(dam, 21) +
                L(dam, 22) + L(dam, 23) + L(dam, 24)+L(dam,25))
summary(fit24)


bic <- function(model)
{
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  return(
    round(c("p" = npar - 1,
            "N"=t,
            "BIC" = log(ssr/t) + npar * log(t)/t,
            "R2" = summary(model)$r.squared), 4)
  )
}

bic(fit1)
bic(fit2)
bic(fit6)
bic(fit12)
bic(fit24)
#R^2 più alto per fit24, mentre bic più basso è dato da fit1 e fit24


#check ipotesi su fit1
res=fit1$residuals
mean(res)
#omoschedasticita
x11()
plot(res)
library(whitestrap)
white_test(fit1)
#correlazione
x11()
plot(res[1:17529],res[2:17530])
library(lmtest)
dwtest(fit1)

