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