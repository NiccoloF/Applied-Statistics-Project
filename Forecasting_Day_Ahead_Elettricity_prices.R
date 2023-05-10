################################################################################
#-------------------FORECASTING DAY AHEAD ELETTRICITY PRICES--------------------
################################################################################

library(ggplot2)
library(GGally)
library(tidyverse)
library(patchwork)
library(lubridate, warn.conflicts = FALSE)
library(dplyr)
library(corrplot)
library(lmtest)
library(dynlm)
library(orcutt)
library(sandwich)
library(car)
library(nlWaldTest)
library(forecast)
library(xts)
library(urca)
library(quantmod)
library(fUnitRoots)
library(FinTS)
library(rugarch)
library(strucchange)
library(forecast)
library(tseries)
library(FinTS)
library(fGarch)
library(rugarch)

########################################################
### loading dataset and selecting desired time window ###
########################################################
data <- read.table('ml_data.csv',header = T, sep=",")
summary(data)
head(data)
data <- data[1:17520,]   # taking only 2018 and 2019 
summary(data)
data <- data[,-1]        # removes the fist colunm since it is repetitive 


######################################################
######## Graphical exploration of the dataset #########
######################################################

# Correlation matrix and heat map visualization
library(corrplot)
corr_data <- cor(data)
quartz() #  x11() for windows
heatmap(corr_data)
quartz()#  x11()
corrplot(corr_data, method="color")
graphics.off()

library(lubridate)
# Compute the mean by day to see if there is a seasonal trend in 2018 and 2019
price_daily_2018 <- aggregate(data[1:8760,]$dam , by=list(data[1:8760,]$day,data[1:8760,]$month), FUN = mean)
price_daily_2019 <- aggregate(data[8761:17520,]$dam, by = list(data[8761:17520,]$day,data[8761:17520,]$month), FUN = mean)

#Compute the mean by month to see a seasonal trend for 2018 and 2019
price_monthly_2018 <- aggregate(data[1:8760,]$dam, by=list(data[1:8760,]$month), FUN = mean)
price_monthly_2019 <- aggregate(data[8761:17520,]$dam, by=list(data[8761:17520,]$month), FUN = mean)

# Compute the hourly mean 
price_hourly_2018 <- aggregate(data[1:8760,]$dam, by=list(data[1:8760,]$hour), FUN = mean)
price_hourly_2019 <- aggregate(data[8761:17520,]$dam, by=list(data[8761:17520,]$hour), FUN = mean)

# compute the weekday prices 
price_weekday_2018<- aggregate(data[1:8760,]$dam, by = list(data[1:8760,]$weekday), FUN = mean)
price_weekday_2019<- aggregate(data[8761:17520,]$dam, by = list(data[8761:17520,]$weekday), FUN = mean)

# plot the results:
quartz()
par(mfrow=c(2,2))

plot(price_daily_2018[,3], type="l",main = "Energy prices in 2018 and 2019", ylab = "daily average price", xlab= "days", col="blue")
lines(price_daily_2019[,3], type="l",col="red")
legend("topright", legend=c("2018", "2019"), col=c("blue", "red"),lty=1,lwd=3)

plot(price_monthly_2018, type="l",main="Monthly average price", ylab = "Price", xlab= "month", ylim=c(40, 80),lwd=4, col="blue")
lines(price_monthly_2019, col= "red" ,lwd=5)
legend("topleft", legend = c("2018","2019"), col = c("blue","red"), lty=1,lwd=3)

plot(price_hourly_2018, type="l",main="Hourly average price", ylab = "Price", xlab= "Day", ylim=c(40, 80),lwd=4, col="blue")
lines(price_hourly_2019, col= "red" ,lwd=5)
legend("topleft", legend = c("2018","2019"), col = c("blue","red"), lty=1, lwd=3)
lines(rep(mean(price_hourly_2018$x),24))
lines(rep(mean(price_hourly_2019$x),24))
grid()

plot(price_weekday_2018, type="l",main="Weekday average price", ylab = "Price", xlab= "Weekday", ylim=c(40, 80),lwd=4, col="blue")
lines(price_weekday_2019, type="l", col="red", lwd=5)
legend("topleft", legend = c("2018","2019"), col = c("blue","red"), lty=1, lwd=3)

graphics.off()

rm( price_daily_2018,price_daily_2019,
    price_monthly_2018,price_monthly_2019,
    price_hourly_2018 ,price_hourly_2019,
    price_weekday_2018,price_weekday_2019)


#####################################################
############# Covariates Selection ###################
#####################################################

#removing less significant covariates 
data<-data[,-c(2,3,4,5,6,7,8,9)]   # removing import/ export 
data<-data[,-4]                    # removing day (the day of the month is not relevant)
summary(data)

#hydro, pv, self.consuption --> high correlation between 9 and 10 --> take the mean 
data$geothermal=(data$geothermal10+data$geothermal9)/2
data$thermal=(data$thermal10+data$thermal9)/2
data$hydro=(data$hydro10+data$hydro9)/2
data$pv=(data$pv10+data$pv9)/2
data$self.consuption=(data$self.consumption9+data$self.consumption10)/2
data$wind=(data$wind10+data$wind9)/2
data<-data[,-c(5,6,7,8,9,10,11,12,13,14,15,16)] # removing 9 and 10 

#Creating new variable weekend
data$weekend<-ifelse(data$weekday==0|data$weekday==6,1,0)

# Creating cathegorical value "Season"
#1=winter, 2 =spring 3=summer 4=autumn
season <- rep(0, length(data$month)) # Crea un vettore di zeri della stessa lunghezza di month
for (i in 1:length(data$month)) {
  if (data$month[i] %in% c(12, 1, 2)) {
    season[i] <- 1
  } else if (data$month[i] %in% c(3, 4, 5)) {
    season[i] <- 2
  } else if (data$month[i] %in% c(6, 7, 8)) {
    season[i] <- 3
  } else if (data$month[i] %in% c(9, 10, 11)) {
    season[i] <- 1
  }
}
data$season=season
rm(season,i)
data=data[,-4]    # remove month
# at this point the dataset should have 15 variables

# re-plotting correlation with new dataset
corr_data <- cor(data)
quartz() #  x11() for windows
corrplot(corr_data,method="number")
graphics.off()
#Note: problem -> many correlated variables 

#transform covariates in factors, useful since the relation between hour and price is not linear
# in this way we can differentiate based on what hour's price  we are predicting 
# same arguments for weekend and season
data$hour=as.factor(data$hour)
data$weekend=as.factor(data$weekend)
data$season=as.factor(data$season)

# remove the outliers
#library(DescTools)
#data=data[2:7,]
#winsorized = lapply(data, Winsorize)
  
##########################################################
############# Fitting a Linear Model #####################
##########################################################

fit_lm <- lm(dam ~ Forecasted_load + hour + geothermal + hydro +
                   pv+self.consuption + gas_price + thermal + peak +
                   weekend, data = as.data.frame(data) )

hist(fit_lm$residuals)
#Checking hypothesis
mean(fit_lm$residuals)    #1) Zero mean
quartz() #x11()
plot(fit_lm$residuals)    #2) Homoschedastic
quartz() #x11()           #3)Correlation
plot(fit_lm$residuals[1:length(fit_lm$residuals)-1],fit_lm$residuals[2:length(fit_lm$residuals)])
quartz() #x11()
qqnorm(fit_lm$residuals)  #4)Normality
qqline(fit_lm$residuals)

graphics.off()
# the assumptions are not verified --> we cannot use this model to make confidence intervals
# We try to improve our model by using autoregressive models/ dynamical models 



# visualize hypothetical correlation with a randomic uniform sample 
rand_sel = runif(200)*17520
data$year = c(rep(2018,8760) , rep(2019,8760))
quartz()
par(mfrow=c(2,2))
plot(data$dam[rand_sel],data$Forecasted_load[rand_sel], col = (data$year[rand_sel]-2016),
     main = "Price vs Forecasted load", xlab = "Price", ylab = "Forecasted load", lwd= 2)
# thermal vs price 
plot(data$dam[rand_sel],data$thermal[rand_sel], col = (data$year[rand_sel]-2016),
     main = "Price vs Thermal", xlab = "Price", ylab = "thermal", lwd= 2)
# peak vs price
plot(data$dam[rand_sel],data$peak[rand_sel], col = (data$year[rand_sel]-2016),
     main = "Price vs Peak", xlab = "Price", ylab = "Peak", lwd= 2)
# peak vs gas price 
plot(data$dam[rand_sel],data$gas_price[rand_sel], col = (data$year[rand_sel]-2016),
     main = "Price vs Gas Price", xlab = "Price", ylab = "Gas Price", lwd= 2)

graphics.off()

# improve linear model 


# plot fitted values vs true values 
quartz()
plot(data$dam[1000:1048],type="l",main="True vs Predicted price ", ylab = "Price", xlab= "Time",lwd=2,col="blue")
lines(fit_lm$fitted.values[1000:1048],type="l",lwd=2,col="red")

graphics.off()

##########################################################
########### Checking hypothesis of stationarity ##########
##########################################################

# change all the data into a time series 
data.ts <- ts(data)
gas_price.ts<- ts(data[,7])
dam.ts <- ts(data[,1])
Forecasted_load.ts<- ts(data[,2])
thermal.ts<- ts(data[,9]) 
diff_dam.ts= diff(dam.ts)  # create covariate difference of dam (i.e. y(t)-y(t-1))

#  Graphic view
quartz() #x11()
par(mfrow=c(1,2))
plot(dam.ts, ylab = "dam", col="steelblue",main="DAM Price vs Time", lwd = 2) # variability seems to be not constant
plot(diff_dam.ts, ylab = "diff dam", col="steelblue",main="Diff_DAM Price vs Time", lwd = 2, ylim = c(-75,75)) 
abline(h = 0, lty = 2, col = "red")
graphics.off()

# break point research ( ricontrollare perche non utile )
fs.dam <- Fstats(dam ~ 1)         # SLOW COMMAND, TAKES A LOT OF TIME
sctest(fs.dam) # Chow-test        
#  very low p-value (e-16) ==>we reject the null hypotesis: there is only one model
#  we conclude that the relationship between the variables in the regression models changes across time 

# Check the stationarity of dam and diff_dam with:
# Dickey-Fuller test    (  H0: trend   vs   H1: not trend / stationarity)
summary(ur.df(dam.ts))         #p-value: < 2.2e-16       (alternative command adf.test(dam)$p.value)
summary(ur.df(diff_dam.ts))        # p-value: < 2.2e-16 note the test statistic is much higher -> smaller p-value
# because of of the low p-value we can reject the null hypothesis H0, so we can assume the stationarity (not unit root)
# can use directly from now just dam prices -> no need to use diff_dam

library(urca)
# in alternative: ERS unit root test ( H0: Non-stationary vs H1: stationary )
price.urers <- ur.ers(dam.ts, type="P-test", model="trend")
summary(price.urers) # Cannot reject H0 therefore non stationary process! p-value of 0.08 not too far from 0.05
#let us now try taking the difference (diff_dam)
dprice.urers1 <- ur.ers(diff_dam.ts, type="P-test", model="trend")
summary(dprice.urers1) # Reject H0 -> stationary 
# we have a borderline case see if we can use dam or diff_dam


####################################################################
################# Exploring Autocorrelation ########################
####################################################################
# Let us now analyse the ACF (Autocorrelation Function) and PACF (Partial Autocorrelation Function)
#remember from theory
#             ACF(autocorrelation function)                  PACF(partial autocorrelation function)
# AR       Geometric decaying (exponentially decaying)                p significant lags  
# MA            q significant lags                                     Geometric decaying
# ARMA           Geometric decaying                                    Geometric decaying

# dam
quartz()   #x11()
par(mfrow=c(1,2))
acf(dam.ts, 48, xlim = c(1,48))
acf(dam.ts, 48, type = 'partial',xlim = c(1,48))
# => we can assume y as ARMA(25,24)

# diff_dam
quartz()   #x11()
par(mfrow=c(1,2))
acf(diff_dam.ts, 48, xlim = c(1,48))
acf(diff_dam.ts, 48, type = 'partial',xlim = c(1,48))

graphics.off()

# There is no clear model to use, we try some dynamical models, trying both a "continuous" approach 
#(i.e selecting all lags from 1 to n ) and a "discontinuous" one (i.e. stressing the dependencies from 
# samples from the day/days before at the same time)

################################################
############# AUTOREGRESSIVE MODELS ############
################################################
# Fitting AR models
ar1 <- dynlm(dam.ts~ +L(dam.ts,1))
summary(ar1)      #Adjusted R-squared:  0.881

ar12 <- dynlm(dam.ts ~ L(dam.ts,1) + L(dam.ts, 2) + L(dam.ts, 3) +
                L(dam.ts, 4) + L(dam.ts, 5) + L(dam.ts, 6) + L(dam.ts,7) + L(dam.ts, 8)+ L(dam.ts, 9) +
                L(dam.ts, 10) + L(dam.ts, 11) + L(dam.ts, 12))
summary(ar12)     #Adjusted R-squared:  0.9048

ar25 <- dynlm(dam.ts~ L(dam.ts,1)+ L(dam.ts, 2)+ L(dam.ts, 3) +
                L(dam.ts, 4) + L(dam.ts, 5) + L(dam.ts, 6)+L(dam.ts,7) + L(dam.ts, 8) + L(dam.ts, 9) +
                L(dam.ts, 10) + L(dam.ts, 11) + L(dam.ts, 12) + L(dam.ts, 13) + L(dam.ts, 14)+ L(dam.ts, 15) +
                L(dam.ts, 16) + L(dam.ts, 17) + L(dam.ts, 18) + L(dam.ts,19) + L(dam.ts, 20)+ L(dam.ts, 21) +
                L(dam.ts, 22) + L(dam.ts, 23) + L(dam.ts, 24) + L(dam.ts,25)) # circa 15 secondi
summary(ar25)

#evaluate the AR models 
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

bic(ar1) 
bic(ar12) 
bic(ar25)


# Fit ARMA model
p <- 24; # degree of autoregressive part
d <- 1  # degree of difference (assume 0/1 for now) -> working on the difference y(t) - y(t-1)
q <- 1; # degree of MA part
arma_model <- arima(dam.ts, order=c(p,d,q)) # very slow command, and same problem as always 
                                          # for the 24 step prediction 
summary(arma_model)
# check the residuals 
plot(arma_model$residuals)
qqnorm(arma_model$residuals)
qqline(arma_model$residuals, col="red")

# to do list check omoschedasticity and normality 

# we would like to see if we are able to improve our model by exploiting all the informations contained
# in our dataset adding soma exogenous variables in dynamical models


###############################################################
#################### Dynamical models##########################
###############################################################
library(dynlm)
# use the variable 

# Try fitting progressively more complex models with the exogenous variables:
fit1 <- dynlm( dam.ts ~ Forecasted_load.ts + gas_price.ts + L(dam.ts,1) + thermal.ts)  # the same with or without thermal 
summary(fit1)

fit2 <- dynlm( dam.ts ~ Forecasted_load.ts + gas_price.ts + L(dam.ts,1) + L(dam.ts, 2) ) 
summary(fit2)

fit12<- dynlm(dam.ts ~ Forecasted_load.ts + gas_price.ts + L(dam.ts,1) + L(dam.ts, 2) + L(dam.ts, 3) +
                L(dam.ts, 4) + L(dam.ts, 5) + L(dam.ts, 6) + L(dam.ts,7) + L(dam.ts, 8)+ L(dam.ts, 9) +
                L(dam.ts, 10) + L(dam.ts, 11) + L(dam.ts, 12))
summary(fit12)


fit25<- dynlm( dam.ts ~ Forecasted_load.ts + gas_price.ts + L(dam.ts,1) + L(dam.ts, 2) + L(dam.ts, 3) +
                        L(dam.ts, 4) + L(dam.ts, 5) + L(dam.ts, 6) + L(dam.ts,7) + L(dam.ts, 8)+ L(dam.ts, 9) +
                        L(dam.ts, 10) + L(dam.ts, 11) + L(dam.ts, 12) + L(dam.ts, 13) + L(dam.ts, 14)+ L(dam.ts, 15) +
                        L(dam.ts, 16) + L(dam.ts, 17) + L(dam.ts, 18) + L(dam.ts,19) + L(dam.ts, 20)+ L(dam.ts, 21) +
                        L(dam.ts, 22) + L(dam.ts, 23) + L(dam.ts, 24) + L(dam.ts,25))
summary(fit25)
# we notice as some of these covariates are not significant for the analysis (i.e high p-value) 
# we try to take the most significant and relevant lags skipping the intermediate times and using only the 
# prices of the days before without the data in between 

fit.1.24 <- dynlm(dam.ts ~ Forecasted_load.ts + gas_price.ts + thermal.ts + L(dam.ts,1)+ L(dam.ts, 24))
summary(fit.1.24)

fit.1.24.48 <- dynlm(dam.ts ~ Forecasted_load.ts + gas_price.ts + thermal.ts + L(dam.ts,1) + L(dam.ts, 24)+L(dam.ts,48))
summary(fit.1.24.48)

fit.1.2.24.48 <- dynlm(dam.ts ~ Forecasted_load.ts + gas_price.ts + thermal.ts + L(dam.ts,1) + L(dam.ts,2) + L(dam.ts, 24) + L(dam.ts,48))
summary(fit.1.2.24.48)

fit.1.2.24.168 <- dynlm(dam.ts ~ Forecasted_load.ts + gas_price.ts + thermal.ts + L(dam.ts,1) + L(dam.ts,2) + L(dam.ts, 24)+L(dam.ts,168))
summary(fit.1.2.24.168)   # note how exogenous variables become more relevant 

# evaluate these models 
bic(fit1)
bic(fit2)
bic(fit6)
bic(fit12)
bic(fit25)
bic(fit_1_24)
bic(fit.1.24.48)
bic(fit.1.2.24.48)
#R^2 più alto per fit24, mentre bic più basso è dato da fit1 e fit24

## try model directly from what we know for prediction
fit_for_pred <- dynlm(dam.ts  ~ data$weekend + gas_price.ts + thermal.ts + L(dam.ts,24)+ L(dam.ts, 48) + L(dam.ts,168) ) 
plot(fit_for_pred$residuals)
bic(fit_for_pred)


quartz()
par(mfrow=c(1,3))
plot(fit_for_pred$residuals )
hist(fit_for_pred$residuals)
qqnorm(fit_for_pred$residuals)
qqline(fit_for_pred$residuals, col="red")

quartz()
plot(dam.ts[950:1100],type="l",main="True vs Predicted price ", ylab = "Price", xlab= "Time",lwd=1,col="blue")
lines(fit_for_pred$fitted.values[950:1100],type="l",lwd=1,col="red")
legend("topright", legend=c("True Values", "Fittet Values"), col=c("blue", "red"),lty=1,lwd=3)
#  note during the week everything is fine, weekends and mondays behaves differently since they are basted
#  on previous days... 

graphics.off()

#check hyp on fit1
mean(fit_for_pred$residuals)
quartz() #x11()
plot(fit_for_pred$residuals) #omoschedasticità
library(whitestrap)
white_test(fit1) 
white_test(fit25)
white_test(fit.1.2.24.168)   # the residuals are not homoschedastic
#correlation
x11()
plot(fit1$residuals[1:17529],fit1$residuals[2:17530])
library(lmtest)
#Durbin-Watson test: low p-value --> true autocorrelation is greater than 0
dwtest(fit1) 

# since the residuals are not homoschedastic and there is statistical evidence of autocorrelation 
# we try to fit ARCH and Garch models


#######################################################################
######################### ARCH and GARCH ##############################
#######################################################################
# another test to see if there is need of a arch model  
archTest.dam <- ArchTest(dam.ts)
archTest.diff_dam <- ArchTest(diff_dam.ts)
archTest.dam              # small p-value --> reject H0 :  NO ARCH effect
archTest.diff_dam         # small p-value

ehatsq <- ts(resid(fit_for_pred)^2)    # store the square of the residuals as a time series 

# regress squared residuals
mod_arch1 <- dynlm(ehatsq ~ L(ehatsq,1), data = ehatsq) #  ARCH(1)
summary(mod_arch1)
bic(mod_arch1) # 10.9433

mod_arch2 <- dynlm(ehatsq ~  L(ehatsq,1) +L(ehatsq,2), data = ehatsq)
summary(mod_arch2)
bic(mod_arch2) # 10.9006 

mod_arch3 <- dynlm(ehatsq ~ L(ehatsq,1) + L(ehatsq,2) + L(ehatsq,3), data = ehatsq)
summary(mod_arch3)
bic(mod_arch3)[3] # 10.8909

mod_arch4 <- dynlm(ehatsq ~ L(ehatsq,1) + L(ehatsq,2) + L(ehatsq,3) + L(ehatsq,4) , data = ehatsq)
summary(mod_arch4)
bic(mod_arch4)[3] # 10.8915  

mod_arch24 <- dynlm(ehatsq ~ L(ehatsq,1) + L(ehatsq,2) + L(ehatsq,3) +
                      L(ehatsq,4) + L(ehatsq,5) + L(ehatsq,6) + L(ehatsq,7) +
                      L(ehatsq,8) + L(ehatsq,9) + L(ehatsq,10) +L(ehatsq,11) + 
                      L(ehatsq,12) + L(ehatsq,13) + L(ehatsq,14) + L(ehatsq,15) +
                      L(ehatsq,16) + L(ehatsq,17) + L(ehatsq,18) + L(ehatsq,19) +
                      L(ehatsq,20) + L(ehatsq,21) + L(ehatsq,22) + L(ehatsq,23) +
                      L(ehatsq,24), data = ehatsq) # circa 15 secondi
summary(mod_arch24)
bic(mod_arch24)[3] # 10.8022 

mod_arch25 <- dynlm(ehatsq ~ L(ehatsq,1) + L(ehatsq,2) + L(ehatsq,3) +
                      L(ehatsq,4) + L(ehatsq,5) + L(ehatsq,6) + L(ehatsq,7) +
                      L(ehatsq,8) + L(ehatsq,9) + L(ehatsq,10) +L(ehatsq,11) + 
                      L(ehatsq,12) + L(ehatsq,13) + L(ehatsq,14) + L(ehatsq,15) +
                      L(ehatsq,16) + L(ehatsq,17) + L(ehatsq,18) + L(ehatsq,19) +
                      L(ehatsq,20) + L(ehatsq,21) + L(ehatsq,22) + L(ehatsq,23) +
                      L(ehatsq,24) + L(ehatsq,25), data = ehatsq)
summary(mod_arch25)
bic(mod_arch25)[3] # 10.7507 ==> best one

mod_arch26 <- dynlm(ehatsq ~ L(ehatsq,1) + L(ehatsq,2) + L(ehatsq,3) +
                      L(ehatsq,4) + L(ehatsq,5) + L(ehatsq,6) + L(ehatsq,7) +
                      L(ehatsq,8) + L(ehatsq,9) + L(ehatsq,10) +L(ehatsq,11) + 
                      L(ehatsq,12) + L(ehatsq,13) + L(ehatsq,14) + L(ehatsq,15) +
                      L(ehatsq,16) + L(ehatsq,17) + L(ehatsq,18) + L(ehatsq,19) +
                      L(ehatsq,20) + L(ehatsq,21) + L(ehatsq,22) + L(ehatsq,23) +
                      L(ehatsq,24) + L(ehatsq,25) + L(ehatsq,26), data = ehatsq)
summary(mod_arch26)
bic(mod_arch26)[3] # 10.7511


# (-) Definition of GARCH model
# usare altro comando!
arch.fit = garchFit(~arma(2,1)+garch(1,1), data = dam, trace = F)
summary(arch.fit)

# WARING: very slow for high orders
spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                  variance.model = list(model = "sGARCH", submodel = 'GARCH',garchOrder = c(2, 0))) 



#########################################
############# CLOSING REMARKS############
#########################################
# we have found models with high R^2 

# To do List :


# understand for prediction 24 step ahead vs 24 different models
# take into account the weekends and treat them accordingly, most of the errors is there 
# better understanding hypothesis of every test used and implications
# see if arch / garch models can be used in prediction, not dependent on the past 23 hours 
# vettori autoregressivi?? 
# anova: stagionalità stagioni, ore, weekend si aggiunge se poi si usa

# CROSS-VALIDATION??
# idea for validation: we can randomically sample two/one week/s, fit the model of the data without those 
# two weeks and then test it 
# another possible approach would be: use only data from the beginning to the chosen week, in this way we
# would be testing what would have happened if we had used our model in  that period of time without knowing
# how the market whould have behaved afterward.

# (for applied) regression trees

