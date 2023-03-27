################################################################################
#-------------------FORECASTING DAY AHEAD ELETTRICITY PRICES--------------------
################################################################################

# Import data_set 
Electricity_price <- read.csv("./Data_set/ml_data.csv", header = TRUE, 
                              row.names = 1)
head(Electricity_price)
dim(Electricity_price)
str(Electricity_price)
summary(Electricity_price)


colnames(Electricity_price)
colnames(Electricity_price)[1] <- "Price"
colnames(Electricity_price)

Electricity_price_2019 <- Electricity_price[row.names(Electricity_price) < "2020-01-01 00:00:00",]

corr <- cor(Electricity_price)
x11()
heatmap(corr)
title(main = "Correlation whole dataset")

corr_2019 <- cor(Electricity_price_2019)
x11()
heatmap(corr_2019)
title(main="Correlation up to 2019 end")

x11()
boxplot(scale(Electricity_price))



