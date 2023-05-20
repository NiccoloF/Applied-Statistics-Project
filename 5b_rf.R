# train a random forest model
rf_model <- randomForest(dam ~ ., data = train)
summary(rf_model)

# predict the test set
pred <- predict(rf_model, test)

# plot the results
x11()
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

# cumpute mse
mse <- mean((test$dam - pred)^2)
mse

# try to predict the whole dataset using the random forest model
pred <- predict(rf_model, data_18_19)

# plot the results
x11()
par(mar = c(1, 1, 1, 1))
plot(data_18_19$dam, pred, xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red")
