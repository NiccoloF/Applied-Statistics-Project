### Forecasting Electricity price ###

library(ggplot2)
library(GGally)
library(tidyverse)
library(patchwork)
library(lubridate, warn.conflicts = FALSE)
library(dplyr)
library(corrplot)

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

#ho tolto peak perchè era molto correlata, non benissimo perchè è significativa
#per la regressione si può fare di meglio nella scelta delle covariate
fit <- lm(dam~Forecasted_load+hour+geothermal+hydro+pv+self.consuption+wind+gas_price+thermal+peak)
summary(fit)
res=fit$residuals
#verifico le ipotesi
#1)
mean(res)
#2) Homoschedastic
x11()
plot(res)
#3)Correlation
x11()
plot(res[1:length(res)-1],res[2:length(res)])
#4)Normality
x11()
qqnorm(res)
qqline(res)