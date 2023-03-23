################################################################################
#-------------------FORECASTING DAY AHEAD ELETTRICITY PRICES--------------------
################################################################################

# Import data_set 
Electricity_price <- read.csv("./Data_set/ml_data.csv", header = TRUE, row.names = 1)
head(Electricity_price)
dim(Electricity_price)
str(Electricity_price)
summary(Electricity_price)