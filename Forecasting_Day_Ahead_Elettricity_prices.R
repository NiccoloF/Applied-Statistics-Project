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


# Correlation matrix and heat map visualization
corr <- cor(Electricity_price)
x11() #IF WORKING ON MAC BASAED SYSTEMS CHANGE ALL x11 OCCURRENCIES WITH QUARTZ
heatmap(corr)

# Split data_set in training and test set
set.seed(123)
train <- sample(1:nrow(Electricity_price), 0.8*nrow(Electricity_price))
train_set <- Electricity_price[train,]
test_set <- Electricity_price[-train,]

# Fit a linear model
lm.fit <- lm(Price ~ ., data = train_set)
summary(lm.fit)

# Predictions
lm.pred <- predict(lm.fit, test_set)
lm.pred <- as.data.frame(lm.pred)
lm.pred <- cbind(lm.pred, test_set$Price)
colnames(lm.pred) <- c("Predicted", "Actual")
head(lm.pred)

# Plot predictions
x11()
plot(lm.pred$Actual, lm.pred$Predicted, xlab = "Actual", ylab = "Predicted")
abline(0, 1)

# Compute RMSE
lm.rmse <- sqrt(mean((lm.pred$Actual - lm.pred$Predicted)^2))
lm.rmse

# Fit a random forest model
library(randomForest)
rf.fit <- randomForest(Price ~ ., data = train_set, ntree = 1000)
rf.fit

# Predictions
rf.pred <- predict(rf.fit, test_set)
rf.pred <- as.data.frame(rf.pred)
rf.pred <- cbind(rf.pred, test_set$Price)
colnames(rf.pred) <- c("Predicted", "Actual")
head(rf.pred)

# Plot predictions
x11()
plot(rf.pred$Actual, rf.pred$Predicted, xlab = "Actual", ylab = "Predicted")
abline(0, 1)

# Compute RMSE
rf.rmse <- sqrt(mean((rf.pred$Actual - rf.pred$Predicted)^2))
rf.rmse

# Fit a gradient boosting model
library(gbm)
gbm.fit <- gbm(Price ~ ., data = train_set, distribution = "gaussian", 
               n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)
gbm.fit

# Predictions
gbm.pred <- predict(gbm.fit, test_set, n.trees = 1000)
gbm.pred <- as.data.frame(gbm.pred)
gbm.pred <- cbind(gbm.pred, test_set$Price)
colnames(gbm.pred) <- c("Predicted", "Actual")
head(gbm.pred)

# Plot predictions
x11()
plot(gbm.pred$Actual, gbm.pred$Predicted, xlab = "Actual", ylab = "Predicted")
abline(0, 1)

# Compute RMSE
gbm.rmse <- sqrt(mean((gbm.pred$Actual - gbm.pred$Predicted)^2))
gbm.rmse

# Fit a neural network model
library(neuralnet)
nn.fit <- neuralnet(Price ~ ., data = train_set, hidden = 5, 
                    linear.output = FALSE)
nn.fit

# Predictions
nn.pred <- compute(nn.fit, test_set[, -1])
nn.pred <- as.data.frame(nn.pred$net.result)
nn.pred <- cbind(nn.pred, test_set$Price)
colnames(nn.pred) <- c("Predicted", "Actual")
head(nn.pred)

# Plot predictions
x11()
plot(nn.pred$Actual, nn.pred$Predicted, xlab = "Actual", ylab = "Predicted")
abline(0, 1)

# Compute RMSE
nn.rmse <- sqrt(mean((nn.pred$Actual - nn.pred$Predicted)^2))
nn.rmse

# Fit a support vector machine model
library(e1071)
svm.fit <- svm(Price ~ ., data = train_set)
svm.fit

# Predictions
svm.pred <- predict(svm.fit, test_set)
svm.pred <- as.data.frame(svm.pred)
svm.pred <- cbind(svm.pred, test_set$Price)
colnames(svm.pred) <- c("Predicted", "Actual")
head(svm.pred)

# Plot predictions
x11()
plot(svm.pred$Actual, svm.pred$Predicted, xlab = "Actual", ylab = "Predicted")
abline(0, 1)

# Compute RMSE
svm.rmse <- sqrt(mean((svm.pred$Actual - svm.pred$Predicted)^2))
svm.rmse

# Fit a k-nearest neighbors model
library(class)
knn.fit <- knn(train = train_set[, -1], test = test_set[, -1], 
               cl = train_set$Price, k = 5)
knn.fit <- as.data.frame(knn.fit)
knn.fit <- cbind(knn.fit, test_set$Price)
colnames(knn.fit) <- c("Predicted", "Actual")
head(knn.fit)

# Plot predictions
x11()
plot(knn.fit$Actual, knn.fit$Predicted, xlab = "Actual", ylab = "Predicted")
abline(0, 1)

# Compute RMSE
knn.rmse <- sqrt(mean((knn.fit$Actual - knn.fit$Predicted)^2))
knn.rmse
