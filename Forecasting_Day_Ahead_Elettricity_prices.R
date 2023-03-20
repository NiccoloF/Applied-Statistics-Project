################################################################################
#-------------------FORECASTING DAY AHEAD ELETTRICITY PRICES--------------------
################################################################################

# Import data_set 
Elettricity_price <- read.csv("./Data_set/ml_data.csv", header = TRUE)
head(Elettricity_price)
dim(Elettricity_price)
str(Elettricity_price)

