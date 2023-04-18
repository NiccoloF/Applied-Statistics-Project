################################################################################
#-------------------FORECASTING DAY AHEAD ELETTRICITY PRICES--------------------
################################################################################

# Import data_set 
Electricity_price <- read.csv("./Data_set/ml_data.csv", header = TRUE, row.names = 1)
head(Electricity_price)
dim(Electricity_price)
str(Electricity_price)
summary(Electricity_price)

# extract 2018-2019

Electricity_price[1:17520,] -> test
head(test)
dim(test)
str(test)
summary(test)
dimnames(test)

quartz()
boxplot(test)
plot(test$dam,test$Swiss_import, col = 'blue')


# heat map
corr <- cor(Electricity_price)
quartz()
par(mar = c(100,4,4,2))
heatmap(corr)

# spiegazione nomi colonne
# dam Day Ahead Market cioè il prezzo [...]
# Swiss_import , export ecc
# forecasted_load produzione prevista
# hour, day month da togliere?? boh no son molto rilevanti
# geothermal10: produzione nostra misurata alle 10am del giorno stesso
# geothermal : geotermica (acqua termale, calore proveniente dal sottosuolo)
# hydro: centrali idroelettriche
# pv : fotovoltaico (non delle case)
# self-consumption : quanto della propria produzione (non solo fotovoltaico) self consumato
# thermal : carbone
# wind : pale eoliche
# peak : massimo (di cosa??)
# valley : minimo idem
# gas_price : prezzo gas
qqnorm(test$dam)
qqline(test$dam, col = 'red', lwd = '2')
shapiro.test(test$dam)
library(nortest)
ad.test(test$dam) # non è gaussiana per niente
col12 <- test[,c(1,2)]
col13 <- test[,c(1,3)]
par(mfrow=c(1,2))
boxplot(col12)
pairs(col12)

plot(col12, col = 'gold')
plot(col13)
