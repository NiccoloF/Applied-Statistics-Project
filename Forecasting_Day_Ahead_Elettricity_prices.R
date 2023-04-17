################################################################################
#-------------------FORECASTING DAY AHEAD ELETTRICITY PRICES--------------------
################################################################################

# Install libraries
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("fpp2")
install.packages("fpp3")
install.packages("forecastHybrid")
install.packages("forecastML")
install.packages("forecastMLExtra")
install.packages("forecastMLData")
install.packages("forecastMLPlots")
install.packages("forecastMLUtils")

# Import libraries
library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)
library(fpp2)
library(fpp3)
library(forecastHybrid)
library(forecastML)
library(forecastMLExtra)
library(forecastMLData)
library(forecastMLPlots)
library(forecastMLUtils)

# Import data_set
data <- read.csv("./Data_set/ml_data.csv",
    header = TRUE,
    row.names = 1
)
head(data)
dim(data)
str(data)
summary(data)

# take only the first two years of the data set
data_18_19 <- data[which((rownames(data) >= "2018-01-01 00:00:00" &
    rownames(data) <= "2019-12-31 23:00:00")), ]
head(data_18_19)
dim(data_18_19)
str(data_18_19)
summary(data_18_19)

# pca on dataset with the first two years
pca <- princomp(data_18_19, scores = TRUE)
pca
summary(pca)

# plot the loadings
loads <- pca$loadings
x11()
par(mar = c(1, 1, 1, 1))
par(mfcol = c(8, 4))
for (i in seq_len(ncol(data_18_19))) {
    barplot(loads[, i], ylim = c(-1, 1), main = paste("PC", i))
}

# plot the explained variance
x11()
par(mar = c(1, 1, 1, 1))
layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))
plot(pca, las = 2, main = "Principal components")
barplot(sapply(data_18_19, sd)^2,
    las = 2,
    main = "Original Variables",
    ylab = "Variances"
)
plot(cumsum(pca$sd^2) / sum(pca$sd^2),
    type = "b", axes = FALSE, xlab = "number of components",
    ylab = "contribution to the total variance", ylim = c(0, 1)
)
abline(h = 1, col = "blue")
abline(h = 0.8, lty = 2, col = "blue")
box()
axis(2, at = 0:10 / 10, labels = 0:10 / 10)
axis(1,
    at = seq_len(ncol(data_18_19)),
    labels = seq_len(ncol(data_18_19)), las = 2
)

# scale the data
data_18_19_sd <- scale(data_18_19)
data_18_19_sd <- as.data.frame(data_18_19_sd)
head(data_18_19_sd)

# pca on scaled dataset with the first two years
pca_sd <- princomp(data_18_19_sd, scores = TRUE)
pca_sd
summary(pca_sd)

# plot the loadings
loads_sd <- pca_sd$loadings
x11()
par(mar = c(1, 1, 1, 1))
par(mfcol = c(8, 4))
for (i in seq_len(ncol(data_18_19_sd))) {
    barplot(loads_sd[, i], ylim = c(-1, 1), main = paste("PC", i))
}

# plot the explained variance
x11()
par(mar = c(1, 1, 1, 1))
layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))
plot(pca_sd, las = 2, main = "Principal components")
barplot(sapply(data_18_19_sd, sd)^2,
    las = 2,
    main = "Original Variables",
    ylab = "Variances"
)
plot(cumsum(pca_sd$sd^2) / sum(pca_sd$sd^2),
    type = "b", axes = FALSE, xlab = "number of components",
    ylab = "contribution to the total variance", ylim = c(0, 1)
)
abline(h = 1, col = "blue")
abline(h = 0.8, lty = 2, col = "blue")
box()
axis(2, at = 0:10 / 10, labels = 0:10 / 10)
axis(1,
    at = seq_len(ncol(data_18_19_sd)),
    labels = seq_len(ncol(data_18_19_sd)), las = 2
)

# split second data frame into train and test randomly
set.seed(2108)
train <- sample(seq_len(nrow(data_18_19)), 0.8 * nrow(data_18_19))
test <- setdiff(seq_len(nrow(data_18_19)), train)
train <- data_18_19[train, ]
test <- data_18_19[test, ]
head(train)
dim(train)
str(train)
summary(train)
head(test)
dim(test)
str(test)
summary(test)

# train a linear regression model
lin_model <- lm(dam ~ ., data = train)
summary(lin_model)
lin_model <- step(lin_model, direction = "both")
summary(lin_model)

# predict the test set
pred <- predict(lin_model, test)

# plot the results
x11()
par(mar = c(1, 1, 1, 1))
plot(test$dam, pred, xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red")

# compute the error
error <- test$dam - pred
mean(error)
sd(error)

# plot the error
x11()
par(mar = c(1, 1, 1, 1))
hist(error, main = "Error", xlab = "Error")

# identify outliers and influence points
cooks_dist <- cooks.distance(lin_model)
res_norm <- rstandard(lin_model)
res_stud <- rstudent(lin_model)

# remove outliers and influence points from the dataset
train <- train[-which(cooks_dist > 4 / (n - p)), ]
train <- train[-which(abs(res_norm) > 2), ]
train <- train[-which(abs(res_stud) > 2), ]
dim(train)

# train a linear regression model
lin_model <- lm(dam ~ ., data = train)
summary(lin_model)
lin_model <- step(lin_model, direction = "both")
summary(lin_model)

# predict the test set
pred <- predict(lin_model, test)

# plot the results
x11()
par(mar = c(1, 1, 1, 1))
plot(test$dam, pred, xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red")

# compute the error
error <- test$dam - pred
mean(error)
sd(error)

# plot the error
x11()
par(mar = c(1, 1, 1, 1))
hist(error, main = "Error", xlab = "Error")

# pca on the dataset leaving out dam
pca_sub <- princomp(data_18_19[, -1], scores = TRUE)
pca_sub
summary(pca_sub)

# plot the loadings
loads_sub <- pca_sub$loadings
x11()
par(mar = c(1, 1, 1, 1))
par(mfcol = c(8, 4))
for (i in seq_len(ncol(data_18_19))) {
    barplot(loads_sub[, i], ylim = c(-1, 1), main = paste("PC", i))
}

# plot the explained variance
x11()
par(mar = c(1, 1, 1, 1))
layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))
plot(pca_sub, las = 2, main = "Principal components")
barplot(sapply(data_18_19, sd)^2,
    las = 2,
    main = "Original Variables",
    ylab = "Variances"
)
plot(cumsum(pca_sub$sd^2) / sum(pca_sub$sd^2),
    type = "b", axes = FALSE, xlab = "number of components",
    ylab = "contribution to the total variance", ylim = c(0, 1)
)
abline(h = 1, col = "blue")
abline(h = 0.8, lty = 2, col = "blue")
box()
axis(2, at = 0:10 / 10, labels = 0:10 / 10)
axis(1,
    at = seq_len(ncol(data_18_19)),
    labels = seq_len(ncol(data_18_19)), las = 2
)

# pca on the dataset leaving out dam and sd
pca_sub_sd <- princomp(data_18_19_sd[, -1], scores = TRUE)
pca_sub_sd
summary(pca_sub_sd)

# plot the loadings
loads_sub_sd <- pca_sub_sd$loadings
x11()
par(mar = c(1, 1, 1, 1))
par(mfcol = c(8, 4))
for (i in seq_len(ncol(data_18_19_sd))) {
    barplot(loads_sub_sd[, i], ylim = c(-1, 1), main = paste("PC", i))
}

# plot the explained variance
x11()
par(mar = c(1, 1, 1, 1))
layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))
plot(pca_sub_sd, las = 2, main = "Principal components")
barplot(sapply(data_18_19_sd, sd)^2,
    las = 2,
    main = "Original Variables",
    ylab = "Variances"
)
plot(cumsum(pca_sub_sd$sd^2) / sum(pca_sub_sd$sd^2),
    type = "b", axes = FALSE, xlab = "number of components",
    ylab = "contribution to the total variance", ylim = c(0, 1)
)
abline(h = 1, col = "blue")
abline(h = 0.8, lty = 2, col = "blue")
box()
axis(2, at = 0:10 / 10, labels = 0:10 / 10)
axis(1,
    at = seq_len(ncol(data_18_19_sd)),
    labels = seq_len(ncol(data_18_19_sd)), las = 2
)

# identify the number of pca that explain at least 80% of the variance
sum(pca_sub_sd$sdev^2) * 0.8
cumsum(pca_sub_sd$sdev^2)
which(cumsum(pca_sub_sd$sdev^2) > sum(pca_sub_sd$sdev^2) * 0.8)[1]

# tranform dataset with the first 10 pc leaving out dam
data_18_19_pca <- predict(pca_sub_sd, data_18_19[, -1])
data_18_19_pca <- data.frame(data_18_19_pca[, 1:10], data_18_19$dam)
colnames(data_18_19_pca) <- c(paste("PC", 1:10, sep = ""), "dam")
head(data_18_19_pca)

# split the dataset into train and test
set.seed(2108)
train_pca <- data_18_19_pca[sample(
    seq_len(nrow(data_18_19_pca)),
    0.8 * nrow(data_18_19_pca)
), ]
test_pca <- data_18_19_pca[-which(
    rownames(data_18_19_pca) %in% rownames(train)
), ]

# train a linear regression model
lin_model_pca <- lm(dam ~ ., data = train_pca)
summary(lin_model_pca)
lin_model_pca <- step(lin_model_pca, direction = "both")
summary(lin_model_pca)

# predict the test set
pred_pca <- predict(lin_model_pca, test_pca)

# plot the results
x11()
par(mar = c(1, 1, 1, 1))
plot(test_pca$dam, pred_pca, xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red")

# compute the error
error_pca <- test_pca$dam - pred_pca
mean(error_pca)
sd(error_pca)

# plot the error
x11()
par(mar = c(1, 1, 1, 1))
hist(error_pca, main = "Error", xlab = "Error")