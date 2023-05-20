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
train <- train[-which(cooks_dist > 4 / nrow(train)), ]
train <- train[-which(abs(res_norm) > 2), ]
train <- train[-which(abs(res_stud) > 2), ]
dim(train)

# train a linear regression model
lin_model <- lm(dam ~ ., data = train)
summary(lin_model)
lin_model <- step(lin_model, direction = "both")
summary(lin_model)

# compute the residuals
res <- residuals(lin_model)

# plot the residuals
x11()
par(mar = c(1, 1, 1, 1))
plot(res, main = "Residuals", xlab = "Residuals")

# verify the normality of the residuals
x11()
ggqqplot(res, main = "Normal Q-Q Plot")

# normality test of the residuals
ks.test(res, "pnorm")

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

# cumpute mse
mse <- mean((test$dam - pred)^2)
mse
