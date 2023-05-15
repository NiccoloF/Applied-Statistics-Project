### Forecasting Electricity price ###

library(ggplot2)
library(GGally)
library(tidyverse)
library(patchwork)
library(lubridate, warn.conflicts = FALSE)
library(dplyr)

# added
library(lmtest)
library(dynlm)
library(orcutt)
library(sandwich)
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
library(dynlm)
library(fGarch)
library(rugarch)

data <- read.table('ml_data.csv',header = T, sep=",")
data <- data[1:17520,]
summary(data)
head(data)
data$weekend <- ifelse(data$weekday == 5 | data$weekday == 6 , 1,0)



#__________________________________________________________________
# Application commands lecture 8 - ARCH/GARCH models
attach(data)
y <- ts(dam)
dy <- diff(y)
detach(data)

# (-) graphic view
x11()
plot(y, ylab = "dam", col="steelblue", lwd = 2)
# variability seems to be not constant

x11()
plot(dy, ylab = "diff dam", col="steelblue", lwd = 2)
abline(h = 0, lty = 2, col = "red")



# - break point research (ricontrollare)
fs.y <- Fstats(y ~ 1) 
sctest(fs.y) # chaw-test
# low p-value ==> reject null hypotesis: there is only one model



# - look at ACF and PACF


#             ACF(autocorrelation function)                  PACF(partial autocorrelation function)

# AR       Geometric decaying (exponentially decaying)                p significant lags  

# MA            q significant lags                                     Geometric decaying

# ARMA           Geometric decaying                                    Geometric decaying

# use this table to decide which model use and alse to find number of lags

# y
x11()
plot(acf(y, 48, xlim = c(1,48)), main = 'acf of y')
# peak at 24 --> MA(24)
x11()
plot(acf(y, 30, type = 'partial',xlim = c(1,30)),main = 'pacf of y')
# significant 25-lag --> AR(25)

# => we can assume y as ARMA(25,24)

# dy
x11()
plot(acf(dy, 40, xlim = c(1,40)), main = 'acf of dy')
# significant 24-lag --> MA part 
x11()
plot(acf(dy, 40, type = 'partial',xlim = c(1,40)), main = 'pacf of dy')
# significant 24-lag --> AR part 

# can be used an ARMA(24,24) for dy


# (-) Dickey-Fuller test
# H0: trend
# H1: not trend / stationarity

adf.test(y)$p.value
# 0.01
# ?
# Warning message:
# In adf.test(y) : p-value smaller than printed p-value
# ?
# as alternative
summary(ur.df(y))
#p-value: < 2.2e-16

# because of of the low p-value we can reject the null hypotesis H0, so we can assume the stationarity (not unit root)
# can use directly from now just y

adf.test(dy)$p.value
summary(ur.df(dy))

# p-value: < 2.2e-16


# (-) Autoregressive model
ar1 <- dynlm(y~ +L(y,1))
summary(ar1)
# The overall model is significative and Adjusted R-squared:  0.881

ar12 <- dynlm(y~ +L(y,1) + L(y,2) + L(y,3) +
                L(y,4) + L(y,5) + L(y,6) + L(y,7) +
                L(y,8) + L(y,9) + L(y,10) +L(y,11) + 
                L(y,12))
summary(ar12)
# The overall model is significative and Adjusted R-squared:  0.9048

ar25 <- dynlm(y~ +L(y,1) + L(y,2) + L(y,3) +
                  L(y,4) + L(y,5) + L(y,6) + L(y,7) +
                  L(y,8) + L(y,9) + L(y,10) +L(y,11) + 
                  L(y,12) + L(y,13) + L(y,14) + L(y,15) +
                  L(y,16) + L(y,17) + L(y,18) + L(y,19) +
                  L(y,20) + L(y,21) + L(y,22) + L(y,23) +
                  L(y,24) + L(y,25)) # circa 15 secondi
summary(ar25)
# The overall model is significative and Adjusted R-squared:  0.9318 


# chose of the order of the model --> use BIC 
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

bic(ar1)[3] # 3.286
bic(ar12)[3] # 3.0687 
bic(ar25)[3] # 2.7411
# lowest BIC -> best model



# (-) test for ARCH effect
# H0: NO ARCH effect
?ArchTest

archTest.y <- ArchTest(y)
archTest.dy <- ArchTest(dy)
archTest.y
archTest.dy
# for both we have a low p-value (=2.2e-16), then we can assume that there is an ARCH effect



# (-) ARCH model

# Step 1:
mod <- dynlm(y ~ 1)
# estimate our first model for y (just the intercept)

# Step 2: 
ehatsq <- ts(resid(mod)^2)
# store the square of the residuals

# Step 3: regress squared residuals
# e.g. ARCH(1)
mod_arch1 <- dynlm(ehatsq ~ L(ehatsq,1), data = ehatsq)
summary(mod_arch1)
bic(mod_arch1)[3] # 10.9433

mod_arch2 <- dynlm(ehatsq ~  L(ehatsq,1) +L(ehatsq,2), data = ehatsq)
summary(mod_arch2)
bic(mod_arch2)[3] # 10.9006 

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

# usare alto comando!
arch.fit = garchFit(~arma(0,0)+garch(1,1), data = y, trace = F)
summary(arch.fit)


# !problema computazionale con ordini alti!
?ugarchspec
spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                  variance.model = list(model = "sGARCH", submodel = 'GARCH',garchOrder = c(25, 0))) # teniamo conto solo dell'effetto arch qui, no MA o AR
?ugarchfit
arch.fit = ugarchfit(data = as.array(y), spec = spec)
arch.fit


# the following table refers to ARMA(1,1) and garch(25,1)

#Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(25,1)
#Mean Model	: ARFIMA(1,0,1)
#Distribution	: norm 

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error    t value Pr(>|t|)
#mu      49.138369    0.562822  87.307139 0.000000
#ar1      0.916141    0.003358 272.838629 0.000000
#ma1      0.327326    0.007429  44.058989 0.000000
#omega    2.894830    0.127536  22.698187 0.000000
#alpha1   0.163410    0.010280  15.895497 0.000000
#alpha2   0.112457    0.009974  11.274810 0.000000
#alpha3   0.017523    0.007342   2.386741 0.016998
#alpha4   0.004621    0.003276   1.410431 0.158413
#alpha5   0.004973    0.003011   1.651929 0.098549
#alpha6   0.027709    0.004158   6.663789 0.000000
#alpha7   0.000000    0.003820   0.000012 0.999991
#alpha8   0.002882    0.003146   0.915973 0.359681
#alpha9   0.000000    0.004842   0.000013 0.999990
#alpha10  0.004633    0.004121   1.124218 0.260921
#alpha11  0.022353    0.005079   4.401366 0.000011
#alpha12  0.057337    0.006215   9.224888 0.000000
#alpha13  0.007513    0.001612   4.660598 0.000003
#alpha14  0.000001    0.003624   0.000148 0.999882
#alpha15  0.000000    0.003566   0.000020 0.999984
#alpha16  0.001721    0.002841   0.605691 0.544720
#alpha17  0.011032    0.004252   2.594255 0.009480
#alpha18  0.000001    0.002497   0.000255 0.999796
#alpha19  0.000000    0.002960   0.000014 0.999989
#alpha20  0.000000    0.004659   0.000001 0.999999
#alpha21  0.000000    0.003555   0.000006 0.999995
#alpha22  0.006441    0.003987   1.615516 0.106199
#alpha23  0.071432    0.007794   9.165344 0.000000
#alpha24  0.385431    0.014519  26.546184 0.000000
#alpha25  0.052523    0.031282   1.679023 0.093148
#beta1    0.000001    0.077885   0.000012 0.999991


# the following table refers to ARMA(0,0) and garch(25,1)

#Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(25,1)
#Mean Model	: ARFIMA(0,0,0)
#Distribution	: norm 

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error    t value Pr(>|t|)
#mu      53.279697    0.130987 406.756463  0.00000
#omega    9.699044    0.765058  12.677532  0.00000
#alpha1   0.792090    0.013466  58.821917  0.00000
#alpha2   0.000000    0.033696   0.000001  1.00000
#alpha3   0.000000    0.009439   0.000013  0.99999
#alpha4   0.004442    0.008114   0.547369  0.58413
#alpha5   0.000003    0.005971   0.000484  0.99961
#alpha6   0.006067    0.005958   1.018241  0.30856
#alpha7   0.000000    0.007071   0.000014  0.99999
#alpha8   0.000000    0.006710   0.000005  1.00000
#alpha9   0.000000    0.005790   0.000004  1.00000
#alpha10  0.000000    0.004939   0.000012  0.99999
#alpha11  0.000216    0.005671   0.038138  0.96958
#alpha12  0.002036    0.004818   0.422528  0.67264
#alpha13  0.000000    0.005731   0.000015  0.99999
#alpha14  0.000000    0.006568   0.000004  1.00000
#alpha15  0.000000    0.005768   0.000005  1.00000
#alpha16  0.000000    0.004294   0.000008  0.99999
#alpha17  0.000000    0.003806   0.000011  0.99999
#alpha18  0.000000    0.004979   0.000006  0.99999
#alpha19  0.000000    0.004408   0.000005  1.00000
#alpha20  0.000000    0.009245   0.000002  1.00000
#alpha21  0.000000    0.010818   0.000004  1.00000
#alpha22  0.000001    0.005030   0.000116  0.99991
#alpha23  0.107461    0.008550  12.569059  0.00000
#alpha24  0.031011    0.001056  29.358350  0.00000
#alpha25  0.000000    0.005796   0.000005  1.00000
#beta1    0.000000    0.045554   0.000001  1.00000


# the following table refers to ARMA(1,1) and garch(25,0)

#Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(25,0)
#Mean Model	: ARFIMA(0,0,0)
#Distribution	: norm 

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error    t value Pr(>|t|)
#mu      53.276676    0.130949 406.850795 0.000000
#omega    9.700657    0.782718  12.393552 0.000000
#alpha1   0.792077    0.016584  47.760783 0.000000
#alpha2   0.000000    0.013353   0.000008 0.999994
#alpha3   0.000000    0.009484   0.000021 0.999983
#alpha4   0.004403    0.008165   0.539253 0.589712
#alpha5   0.000006    0.005976   0.000952 0.999241
#alpha6   0.006074    0.005958   1.019392 0.308017
#alpha7   0.000000    0.007071   0.000022 0.999982
#alpha8   0.000000    0.006760   0.000011 0.999991
#alpha9   0.000000    0.005765   0.000010 0.999992
#alpha10  0.000000    0.004964   0.000027 0.999978
#alpha11  0.000177    0.005676   0.031200 0.975110
#alpha12  0.002064    0.004817   0.428357 0.668391
#alpha13  0.000000    0.005742   0.000035 0.999972
#alpha14  0.000000    0.006571   0.000010 0.999992
#alpha15  0.000000    0.005777   0.000012 0.999990
#alpha16  0.000000    0.004300   0.000019 0.999985
#alpha17  0.000000    0.003804   0.000024 0.999981
#alpha18  0.000000    0.004979   0.000014 0.999989
#alpha19  0.000000    0.004537   0.000014 0.999989
#alpha20  0.000000    0.008916   0.000007 0.999995
#alpha21  0.000000    0.010367   0.000010 0.999992
#alpha22  0.000001    0.005050   0.000242 0.999807
#alpha23  0.107434    0.010120  10.615537 0.000000
#alpha24  0.031016    0.008727   3.554066 0.000379
#alpha25  0.000000    0.006418   0.000012 0.999990





