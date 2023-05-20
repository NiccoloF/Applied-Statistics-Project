# compute the monthly mean of dam in 2018
dam_18_monthly <- aggregate(data_18$dam,
    by = list(month = data_18$month), FUN = mean)
dam_18_monthly <- dam_18_monthly$x - mean(data_18$dam)

# compute the monthlyy mean of dam in 2019
dam_19_monthly <- aggregate(data_19$dam,
    by = list(month = data_19$month), FUN = mean)
dam_19_monthly <- dam_19_monthly$x - mean(data_19$dam)

# compute the daily mean of dam in 2018
dam_18_weekday <- aggregate(data_18$dam,
    by = list(day = data_18$weekday), FUN = mean)
dam_18_weekday <- dam_18_weekday$x - mean(data_18$dam)

# compute the daily mean of dam in 2019
dam_19_weekday <- aggregate(data_19$dam,
    by = list(day = data_19$weekday), FUN = mean)
dam_19_weekday <- dam_19_weekday$x - mean(data_19$dam)

# compute the hourly of dam in 2018
dam_18_hourly <- aggregate(data_18$dam,
    by = list(hour = data_18$hour), FUN = mean)
dam_18_hourly <- dam_18_hourly$x - mean(data_18$dam)

# compute the hourly of dam in 2019
dam_19_hourly <- aggregate(data_19$dam,
    by = list(hour = data_19$hour), FUN = mean)
dam_19_hourly <- dam_19_hourly$x - mean(data_19$dam)

# plot the aggregated means
x11()
par(mfcol = c(2, 2))
boxplot(data_18$dam, data_19$dam, names = c("2018", "2019"),
    horizontal = TRUE,
    main = "Boxplot of dam in 2018 and 2019",
    cex.main = 2.5,
    xlab = "Year", ylab = "Dam", col = c("#009687", "#FB4B41")
)
abline(v = mean(data_18$dam), col = "black", lty = 2)
abline(v = mean(data_19$dam), col = "black", lty = 2)
plot(dam_18_monthly, type = "l",
    col = "#009687",
    main = "Monthly mean of dam in 2018 and 2019",
    cex.main = 2.5,
    xlab = "Month", ylab = "Dam",
    lwd = 5)
lines(dam_19_monthly, col = "#FB4B41", lwd = 5)
legend("topleft", legend = c("2018", "2019"),
    col = c("#009687", "#FB4B41"), lty = 1, cex = 0.8,
    lwd = 5
)
plot(dam_18_weekday, type = "l",
    col = "#009687",
    main = "Daily mean of dam in 2018 and 2019",
    cex.main = 2.5,
    xlab = "Day", ylab = "Dam",
    lwd = 5)
lines(dam_19_weekday, col = "#FB4B41", lwd = 5)
legend("topleft", legend = c("2018", "2019"),
    col = c("#009687", "#FB4B41"), lty = 1, cex = 0.8,
    lwd = 5
)
plot(dam_18_hourly, type = "l",
    col = "#009687",
    main = "Hourly mean of dam in 2018 and 2019",
    cex.main = 2.5,
    xlab = "Hour", ylab = "Dam",
    lwd = 5)
lines(dam_19_hourly, col = "#FB4B41", lwd = 5)
legend("topleft", legend = c("2018", "2019"),
    col = c("#009687", "#FB4B41"), lty = 1, cex = 0.8,
    lwd = 5
)

# print the dam of 2018 and 2019 and the gas price
x11()
plot(data_18_19$dam, type = "l",
    col = "#009687",
    main = "Dam and gas price in 2018 and 2019",
    cex.main = 2.5
)
lines(data_18_19$gas, col = "#FB4B41", lwd = 5)
legend("topleft", legend = c("Dam", "Gas price"),
    col = c("#009687", "#FB4B41"), lty = 1, cex = 0.8,
    lwd = 5
)
