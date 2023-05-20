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
x11()
par(mar = c(1, 1, 1, 1))
par(mfcol = c(2, 5))
for (i in 1:10) {
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
x11()
par(mar = c(1, 1, 1, 1))
par(mfcol = c(2, 5))
for (i in 1:10) {
    barplot(loads[, i], ylim = c(-1, 1), main = paste("PC", i))
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
