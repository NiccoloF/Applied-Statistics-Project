# train a gradient boosted regression tree model
gbm_model <- gbm(dam ~ ., data = train, distribution = "gaussian")
summary(gbm_model)

# predict the test set
pred <- predict(gbm_model, test, n.trees = 1000)

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

# cumpute mse
mse <- mean((test$dam - pred)^2)
mse
