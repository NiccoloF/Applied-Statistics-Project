################################################################################
#-------------------FORECASTING DAY AHEAD ELETTRICITY PRICES--------------------
################################################################################

# Import data_set 
Elettricity_price <- read.csv("/Users/niccoloferesini/Desktop/dam-ahead/ml_data.csv", header = TRUE)
head(Elettricity_price)
dim(Elettricity_price)
str(Elettricity_price)

