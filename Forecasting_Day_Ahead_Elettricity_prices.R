################################################################################
#-------------------FORECASTING DAY AHEAD ELETTRICITY PRICES--------------------
################################################################################

# Import data_set 
Electricity_price <- read.csv("./Data_set/ml_data.csv", header = TRUE, row.names = 1)
head(Electricity_price)
dim(Electricity_price)
str(Electricity_price)
summary(Electricity_price) 

colnames(Electricity_price)
colnames(Electricity_price)[1] <- "Price"
colnames(Electricity_price)

Electricity_price <- Electricity_price[row.names(Electricity_price) < "2020-01-01 00:00:00",]
dim(Electricity_price)


# Correlation matrix and heat map visualization
corr <- cor(Electricity_price)
quartz() #IF WORKING ON MAC BASAED SYSTEMS CHANGE ALL x11 OCCURRENCIES WITH QUARTZ
heatmap(corr)

# compute the mean by day to see if there is a seasonal trend (2018)
library(lubridate)
Electricity_price_2018 <- Electricity_price[row.names(Electricity_price) < "2019-01-01 00:00:00",]
price_daily_2018 <- aggregate(Electricity_price_2018$dam, by = list(Electricity_price_2018$day,Electricity_price_2018$month), FUN = mean)
plot(price_daily_2018[,3], type="l")

# compute the mean by day to see if there is a seasonal trend (2019)
Electricity_price_2019 <- Electricity_price[row.names(Electricity_price) > "2018-12-31 23:00:00",]
price_daily_2019 <- aggregate(Electricity_price_2019$dam, by = list(Electricity_price_2019$day,Electricity_price_2019$month), FUN = mean)
lines(price_daily_2019[,3], type="l",col="red")

# compute the hourly prices 
price_hour_2018<- aggregate(Electricity_price_2018$dam, by = list(Electricity_price_2018$hour), FUN = mean)
plot(price_hour_2018, type="l", ylim=c(35,75))
price_hour_2019<- aggregate(Electricity_price_2019$dam, by = list(Electricity_price_2019$hour), FUN = mean)
lines(price_hour_2019, type="l", col="red")

plot(Electricity_price$hour, Electricity_price$dam)
lines(lines(price_hour_2018, type="l", col="blue"))
