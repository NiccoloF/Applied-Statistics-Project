####################  USEFUL CODE FOR APPLIED STATISTICS #################### 


# OPT + CMD + 0

#### setting directories ####

load('/Users/chiarascrimieri/Desktop/APPLIED STATISTICS/Lab sessions/LAB_5/mcshapiro.test.RData')


setwd('/Users/chiarascrimieri/Desktop/...')

#### LIBRARIES ####

library(car)                 # plotting ellipses, Box-Cox transformation, linearHypothesis()
library(MASS)                # lda(), qda() function, lm.ridge()
library(class)               # knn function
library(glmnet)              # glmnet() function (Ridge, Lasso, Elastic Net)
library(leaps)               # regsubsets()
library(tree)                # tree() function (classification and regression tree
library(mvtnorm)
library(mvnormtest)


data <- read.table('...',header=T)
head(data)


#### PCA ####

head(data)
n <- dim(data)[1]
p <- dim(data)[2]

data.originalmean <- sapply(data, mean)
data.originalsd <- sapply(data, sd)

# checking variability: if there are some variables with a very larger variance, 
# they could driven the analysis of Principal Components:
boxplot(scale(x=data, center = TRUE, scale = FALSE), las = 2, col = 'gold')

# if you want to standardize data:
data <- scale(data)
data <- data.frame(data)

# performing PCA:
pc.data <- princomp(data, scores=T)
summary(pc.data)

# standard deviation of the components (square root of eigenvalues):
pc.data$sd
# proportion of variance explained by each Principal Component: 
pc.data$sd^2/sum(pc.data$sd^2)
# cumulative proportion of explained variance: 
cumsum(pc.data$sd^2)/sum(pc.data$sd^2)

load.data <- pc.data$loadings

# plotting all loadings
n_bar <- ceiling(p/2)
par(mfrow = c(2,n_bar))
for(i in 1:p) barplot(load.data[,i], ylim = c(-1, 1), main=paste('PC',i))

# plotting first 3 loadings
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.data[,i], ylim = c(-1, 1), main = paste('PC', i))


# plotting results (Screeplot on the right)
varmax <- max(var(data[,1:dim(data)[2]]))
varmax_pc <- max(pc.data$sd)
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.data, las=2, main='Principal components', ylim=c(0,varmax_pc^2))
barplot(sapply(data,sd)^2, las=2, main='Original Variables', ylim=c(0,varmax),
        ylab='Variances')
plot(cumsum(pc.data$sd^2)/sum(pc.data$sd^2), type='b', axes=F, 
     xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data),labels=1:ncol(data),las=2)



### Scatterplot of the data along the first two principal components
par(mfrow = c(1,1))
biplot(pc.data)
#OPPURE (sbagliato)
plot(pca$scores[,1:2])
abline(h=0, v=0, lty=2, col='grey')


# Looking at the screeplot, I can notice that the first ### PC's explain about ### of the total variability.
# Moreover, there is an elbow after the ### PC's.  
# These signs suggest me to keep only the first ### PC's to greatly reduce the dimensionality 
# of the data without loosing so much information.

# keep only k components
k <- #**********************************************************************************************************
# Variance explained along the first k PC
(cumsum(pc.data$sd^2)/sum(pc.data$sd^2))[k]
  
# analyze the results 
par(mfrow = c(1,1))
biplot(pc.data)

# project a new point in the reduced space of PC's
new.datum <- c(, , , ) # **********************************************************************************+

# if I have scaled my data, I have to scale even the new datum!
new.datum <- (new.datum - data.originalmean)/data.originalsd

proj <- new.datum %*% load.data

# keep only the first k components:
proj[1:k]




# Normality of the principal components
PCs <- data.frame(princomp(data)$scores)
plot(PCs, asp=1, pch=19)

par(mfcol=c(2,4))
for(i in 1:4)
{
  hist(PCs[,i], prob=T, main=paste('Histogram of PC', i, sep=''))
  lines(seq(min(PCs[,i]), max(PCs[,i]), length=2000), dnorm(seq(min(PCs[,i]), max(PCs[,i]), length=2000),mean(PCs[,i]),sd(PCs[,i])), col='blue', lty=2)
  qqnorm(PCs[,i], main=paste('QQplot of PC', i, sep=''))
  qqline(PCs[,i])
  print(shapiro.test(PCs[,i])$p)
}


# Mahalanobis distances of the data from the sample mean
M <- colMeans(data)
S <- cov(data)

d2 <- matrix(mahalanobis(data, M, S))

par(mfrow=c(1,2))

hist(d2, prob=T)
lines(0:2000/100, dchisq(0:2000/100,4), col='blue', lty=2)

qqplot(qchisq(seq(0.5/30, 1 - 0.5/30 ,by = 1/30), df = 4), d2,  main='QQplot di d2')
abline(0, 1)

d2.class <- cut(d2, qchisq((0:6)/6, df = 4))
d2.freq  <- table(d2.class)

chisq.test(x = d2.freq, p = rep(1/6, 6), simulate.p.value = T)

# test of all the directions simultaneously
mcshapiro.test(data)


### If the data don't seem Gaussian. What can we do?
### Identify clusters 
### Identify (and possibly remove) outliers
### Transform the data (e.g., Box-Cox transformations, see Johnson-Wichern Chap.4.8,
###                     R functions powerTransform(); bcPower())
### Work without the Gaussian assumption (e.g., permutation tests)

### Let's try to identify and remove outliers:
# We remove the data too far (in the sense of the Mahalanobis distance) 
# from the center of the distribution





# PCA VARIABILI CATEGORICHE 


#DATASET NUOVO CON PRINCIPAL COMPONENT CON UN SOLO FACTOR
data_c1<- which(dataor$...=='factor') #cambia il factor*******************************************
scores <- as.data.frame(pc.data$scores)
data <- scores[data_c1,1:2] #cambia se vuoi pi� principal component ********************



# PLOT
dataor <- #dataset originale mai manomesso *********************************
  
  k <- c()
for( i in 1:n){ 
  if(dataor[i,9] == "cannellini")  #SOSTITUISCI A 9 IL NUM DELLA COLONNA CON LA VARIABILE FACTOR + factor *************************************
    k <- cbind(k,1)
  if(dataor[i,9] == "adzuki")  #SOSTITUISCI A 9 IL NUM DELLA COLONNA CON LA VARIABILE FACTOR + factor  *****************************************
    k <- cbind(k,2)
  if(dataor[i,9] == "black-eyed") #SOSTITUISCI A 9 IL NUM DELLA COLONNA CON LA VARIABILE FACTOR + factor **************************************
    k <- cbind(k,3)
}
plot(pc.data$scores[,1],pc.data$scores[,2], col = k , pch = 16, xlab = 'PC1', ylab = 'PC2')
legend('topleft',c('factor1', 'factor2', 'factor3'), col = c('black','red','green'), pch = 16) 




#### GAUSSIANITY ####

# univariate case: 
shapiro.test(data)

# multivariate case:
# compute statistics for all possible directions, performing shapiro.test on the 
# minimum value of the statistics. 
mcshapiro.test(data)

# if pvalue is large, I have no evidence to reject the null hypothesis of Gaussianity.
# if pvalue is small, I reject the hypothesis of Gaussianity.


#Prediction ellipse that contains alpha% of data (you need assumption of Gaussianity): (Ellisse contenente aplha% dei dati )

n <- dim(data)[1]
p <- dim(data)[2]

data.mean <- sapply(data, mean) # mean
data.cov <- cov(data) # var/cov matrix
data.invcov <- solve(data.cov) # inverse of var/cov matrix

alpha <- 0.01 #********************************************************
cfr.fisher <- qchisq(1-alpha,p)

# Center:
center <- data.mean
colnames(center) <- colnames(data.mean)
center

# Directions of the principal axes:
direction <- eigen(data.cov)$vectors
colnames(direction) <- colnames(data)
direction

# # Length of the semi-axes of the ellipse:
semi.axes <- sqrt(cfr.fisher)*sqrt(eigen(data.cov/n)$values)
semi.axes

plot(data, asp = 1,xlim = c(-40,40),ylim = c(-40,40)) # scatterplot of data
points(data.mean[1], data.mean[2], pch = 16, col ='red', cex = 1.5) # sample mean
# plotting Prediction region for data (centered in data.mean)
ellipse(data.mean, data.cov, sqrt(cfr.fisher), col = 'red', lty = 2, lwd=2, center.cex=1)
#dataEllipse(as.matrix(data), levels=0.99, add =T)   Questa ? quella del lab




#### RETRIEVING GAUSSIANITY ####

## Retrieving Gaussianity:
# • Identify clusters, and split the analysis in different clusters;
# • Identify (and possibly remove) outliers;
# • Transform the data (e.g., Box-Cox transformations, see Johnson-Wichern Chap.4.8, R functions pow-
#                         erTransform(); bcPower());
# • Work without the Gaussian assumption (e.g., permutation tests);
# • Asymptotic results (if we have a large dataset and if you need Gaussianity for the sample mean)



## example: identifying outliers
# Mahalanobis distances of the data from the sample mean
d2 <- matrix(mahalanobis(data, colMeans(data), cov(data)))
threshold <- 7.5        # SET THE THRESHOLD ********************************************
plot(d2, pch = ifelse(d2 < threshold, 1, 19)) # plotting the distances to set threshold 
plot(data, pch=ifelse(d2 < threshold, 1, 19)) # plotting original data highlighting outliers
data <- data[which(d2<threshold),]            # removing outliers 
mcshapiro.test(data)                          # checking Gaussianity



## example: univariate Box-Cox transformations
# remember that you can use it only for POSITIVE data, and that:
# • For lambda<1: observations <1 are “spread”, observations >1 are “shrinked” 
# • For lambda>1: observations <1 are “shrinked”, observations >1 are “spread”
# • For lambda=1: no transformation made
# • For lambda=0: log transformation
shapiro.test(data)
lambda.data <- powerTransform(data)           # optimal lambda
bc.data <- bcPower(data, lambda.data$lambda)  # transforming data
shapiro.test(bc.data)


## example: multivariate Box-Cox transformations
# I perform Box-Cox simultaneously on each variable
lambda.data <- powerTransform(data)
lambda.data

# note that if there are some values of lambda that are 
# close to 0 you can approximate them to 0, 
# and similarly with lambda close to 1 for a better interpretation.

# # id_1: id_1-the lambda is close to 1
# lambda.data[id_1] <- 1
# # id_0: id_1-the lambda is close to 0
# lambda.data[id_0] <- 0

bc.data <- data
for (i in 1:dim(data)[2]){
  bc.data[,i] <- bcPower(data[,i], lambda.data$lambda[i])
}

mcshapiro.test(data)













#### TEST FOR THE MEAN - MULTIVARIATE GAUSSIAN ####

# Test on the mean of level $\alpha$ 
# We don't have a large number of data so I need Gaussianity assumption on X. In this way I can use the Hotelling's Theorem
# that assume that T0 is distributed as a Fischer distribution. (Gaussianity assumption is needed)

# \[ H_0: \mu = \mu_0 \quad vs \quad  H_1: \mu \neq \mu_0 \]
# \[ X \sim \mathcal{N}(\mu,\Sigma) \]
# where $\mu_0 = (, , , ...)$  

n <- dim(data)[1]
p <- dim(data)[2]
data.mean <- sapply(data, mean)  # mean
data.cov <- cov(data)            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix

alpha <- 0.05 # SET ALPHA ******************************************************************
mu0 <- c(0,0) # SET MU_0  ******************************************************************



# Comando automatico
var_1 <- data[[1]]
var_2 <- data[[2]]
t.test(var_1, var_2, var.eq=T)

# mean under H0 (blue)
points(mu0[1], mu0[2], col='blue', pch=16)

# sample mean (black)
points(data.mean[1], data.mean[2], col='black', pch=16)

# we represent the confidence region of level 95%: where does mu0 fall?
alpha <- .05
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
ellipse(center=data.mean, shape=data.cov/n, radius=sqrt(cfr.fisher), lwd=2)

# what about the region of level 99%?
# plot(data)    se non va senza!
alpha <- .01
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
ellipse(center=data.mean, shape=data.cov/n, radius=sqrt(cfr.fisher), lwd=2, col='orange', add=TRUE)
radius <- sqrt(cfr.fisher)




# Checking Gaussianity assumption of data: 
mcshapiro.test(data)$pvalue
# High (> 0.1): there is no statistical evidence to reject H0 (I can assume Gaussianity of data)
# Very low (< 0.05): there is statistical evidence to reject H0 (I can NOT assume Gaussianity of data)


# Squared malhanobis distance between the sample mean and the values of the hypothesis mu0
data.T2 <- n * (data.mean-mu0) %*% data.invcov %*% (data.mean-mu0) # T2 statistics


# Radius of the ellipsoid: 
# quantile of the Fisher distribution with p and n-p degrees of freedom at level 1-alpha
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
radius <- sqrt(cfr.fisher)


# Check if mu0 is outside the rejection region {data.T2>cfr.fisher}:
data.T2 < cfr.fisher

# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# Compute the p-value (if alpha not defined)
pvalue <- 1 - pf(data.T2*(n-p)/((n-1)*p), p, n-p)
pvalue
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 



#CONFIDENCE REGION (NO NEED OF mu0):
# Confidence Region for $\boldsymbol{\mu}$ of level $1-\alpha$:
#   \[CR_{1-\alpha}(\boldsymbol{\mu}) = \{ n*(\overline{\mathbf{x}} -\boldsymbol{\mu})'S^{-1}(\overline{\mathbf{x}} -\boldsymbol{\mu}) < cfrFisher \} \]
# 
# where $cfrFisher = \frac{p(n-1)}{n-p} F_{1-\alpha}(p,n-p)$ is the radius of the ellipses. Namely it's an ellipse with:
# 
# - center in $\overline{\mathbf{x}}$
# - direction of principal axes: eigenvector of the covariance matrix divided by n
# - length of the semi-axes: square root of the product between the cfrFisher and the eigenvalues of the covariance matrix divided by n

#Center
data.mean
# Directions of the principal axes:
eigen(data.cov/n)$vectors
# Length of the semi-axes of the ellipse:
sqrt(cfr.fisher)*sqrt(eigen(data.cov/n)$values)

#REJECTION REGION:
# Rejection Region for $\boldsymbol{\mu}$ of level $1-\alpha$:
# \[RR_{1-\alpha}(\boldsymbol{\mu_0}) = \{ n*(\overline{\mathbf{x}} -\boldsymbol{\mu_0})'S^{-1}(\overline{\mathbf{x}} -\boldsymbol{\mu_0}) < cfrFisher \} \]
# where $cfrFisher = \frac{p(n-1)}{n-p} F_{1-\alpha}(p,n-p)$ is the radius of the ellipses. Namely it's an ellipse with:
# 
# - center in $\mathbf{\mu_0}$
# - direction of principal axes: eigenvector of the covariance matrix divided by n
# - length of the semi-axes: square root of the product between the cfrFisher and the eigenvalues of the covariance matrix divided by n
                                                                  
# Center:  
print(paste('Center: ', mu0)) #se non abbiamo mu_0 usare data.mean
# Directions of the principal axes:
print(paste('Directions: ', eigen(data.cov/n)$vectors))
# Length of the semi-axes of the ellipse:
print(paste('Length of the semi-axes: ', sqrt(cfr.fisher)*sqrt(eigen(data.cov/n)$values)))



# PLOT DI REJECTION E CONFIDENCE REGION #puoi anche scegliere una delle due
# plotting: only if dim=2
plot(data, asp = 1) # scatterplot of data
# plotting the Rejection Region (centered in mu0)
ellipse(mu0, shape=data.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
points(data.mean[1], data.mean[2], pch = 16, col ='red', cex = 1.5) # sample mean 
# plotting Confidence region (centered in data.mean)
ellipse(data.mean, data.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, lwd=2, center.cex=1)


##### Elliptical REGION THAT CONTAINS 99% OF DATA -----------

#CONFIDENCE REGION (NO NEED OF mu0):
# Ellipse containing (1-alpha)% of points:
#   \[CR_{1-\alpha}(\boldsymbol{\mu}) = \{ (\overline{\mathbf{x}} -\boldsymbol{\mu})'S^{-1}(\overline{\mathbf{x}} -\boldsymbol{\mu}) < chisq \} \]
# 
# where $chisq = \chi (1-\alpha, p)$ is the radius of the ellipses. Namely it's an ellipse with:
# 
# - center in $\overline{\mathbf{x}}$
# - direction of principal axes: eigenvector of the covariance matrix divided by n
# - length of the semi-axes: square root of the product between the cfrFisher and the eigenvalues of the covariance matrix divided by n

alpha <- 0.01 # SET ALPHA: (0.01 for 99% of data) ******************************
cfr.chisq <- qchisq(1-alpha,p)
# Characterize the ellipse:
# Axes directions:
eigen(data.cov)$vectors
# Center:
data.mean
# Radius of the ellipse:
r <- sqrt(cfr.chisq)
# Length of the semi-axes:
r*sqrt(eigen(data.cov)$values)
# Plot
plot(data, asp = 1, col='forestgreen', pch=19)
points(data.mean[1], data.mean[2], pch = 4, cex = 1.5, lwd = 2)
ellipse(center=data.mean, shape=data.cov, radius=sqrt(cfr.chisq), col = 'black', lty = 2, center.pch = 4)



# Test on linear combination 

a <- c(a_1,a_2) #*****************************
delta.0 <- 0

data <- as.matrix(data)
t.stat <- (mean(data %*% a) - delta.0) / sqrt(var(data %*% a) / n ) # t-statistics (statistica 1!)

## UNILATERAL
# Reject for large values of t 
# => compute the p-value as the probability of the right tail (i.e., of values >tstat)
P <- 1-pt(t.stat, n-1)
P


## BILATERAL
P <- (1-pt(abs(t.stat),n-1))*2
P



#### TEST FOR THE MEAN - CI of level 1-alpha% for the mean of the sum 
a2 <- c(1,1)
alpha <- #****************************************************
cfr.t <- qt(1-alpha/2, n-1)

c(inf = mean(data %*% a2) - cfr.t * sqrt( var(data %*% a2) / n ),
  center = mean(data %*% a2),
  sup = mean(data %*% a2) + cfr.t * sqrt( var(data %*% a2) / n ))

# Otherwise one can use the function t.test()
lc <- data[,1] + data[,2]
t.test(lc, alternative = 'two.sided', mu = 0, conf.level = 0.90)





#### TEST FOR THE MEAN - simultaneous T2 intervals
## If there are two variables and we want a test for each one and for the sum
a1<-c(1,0)
a2<-c(0,1)
a3<-c(1,1)

ICT21<-data.frame(L=t(a1)%*%data.mean-sqrt(t(a1)%*%data.cov%*%a1/n)*sqrt(cfr.fisher),C=t(a1)%*%data.mean,U=t(a1)%*%data.mean+sqrt(t(a1)%*%data.cov%*%a1/n)*sqrt(cfr.fisher))
ICT22<-data.frame(L=t(a2)%*%data.mean-sqrt(t(a2)%*%data.cov%*%a2/n)*sqrt(cfr.fisher),C=t(a2)%*%data.mean,U=t(a2)%*%data.mean+sqrt(t(a2)%*%data.cov%*%a2/n)*sqrt(cfr.fisher))
ICT23<-data.frame(L=t(a3)%*%data.mean-sqrt(t(a3)%*%data.cov%*%a3/n)*sqrt(cfr.fisher),C=t(a3)%*%data.mean,U=t(a3)%*%data.mean+sqrt(t(a3)%*%data.cov%*%a3/n)*sqrt(cfr.fisher))
ICT2<-data.frame(rbind(ICT21,ICT22,ICT23))
ICT2
# Add the comment
for (i in 1:3)
  print(paste('Reject H0 for a',i,': ', !(0>ICT2[i,1] & 0<ICT2[i,3]),sep=''))





#### TEST FOR THE VARIANCE - MULTIVARIATE GAUSSIAN ####
var_1 <- data[[1]]
var_2 <- data[[2]]

var.test(var_1, var_2)



#### TEST FOR THE MEAN - ASYMPTOTIC ####

# No Gaussianity assumption are needed in this case since we use the Central Limit Theorem.
#So TCL and LGN assure that the Mahalanobis distance of the sample mean to the real mean is distributed as a Chi Squared.
# \[H_0: \mu = \mu_0 \quad vs \quad H_1: \mu \neq \mu_0 \]
# with in this case $\mu_0=c(,)$ ******************************************************************

mu0 <- c(, , , ....)   # ******************************************************************
alpha <- 0.05 # ******************************************************************

n <- dim(data)[1]
p <- dim(data)[2]
data.mean <- sapply(data, mean)  # mean
data.cov <- cov(data)            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix

data.T2A <- n * (data.mean-mu0) %*%  data.invcov  %*% (data.mean-mu0)  
cfr.chisq <- qchisq(1-alpha,p)   # radius of the ellipsoid

# Check if mu0 is outside the rejection region {data.T2A>cfr.chisq}:
data.T2A < cfr.chisq 
ifelse(data.T2A < cfr.chisq , 
       "TRUE: there is no statistical evidence to reject H0", 
       "FALSE: there is statistical evidence to reject H0")
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# Compute the p-value (if alpha not defined)
pvalue <- 1-pchisq(data.T2A, p)
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 






#### TEST FOR THE MEAN - SIMULTANEOUS CI ####

# basically they are the projections on specific directions of the ellipsoidal confident region

n <- dim(data)[1]
p <- dim(data)[2]
alpha <- #*******************************
data.mean <- sapply(data, mean)  # mean
data.cov <- cov(data)            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

mu0 <- c(, , , ...) # ******************************************************************

# Checking Gaussianity assumption of data: 
mcshapiro.test(data)

# Simultaneous T2 confidence intervals on the coordinate directions:
T2 <- cbind(inf = data.mean - sqrt(cfr.fisher*diag(data.cov)/n),
            center = data.mean, 
            sup = data.mean + sqrt(cfr.fisher*diag(data.cov)/n))
T2
# if mu0 is contained in each interval, we cannot reject H0. 
# if mu0 is NOT contained in each interval, there is evidence to reject H0. 


# if p=2, you can plot the Sim-CI and the confidence region:
plot(data, asp = 1,main='Confidence and rejection regions')
# rejection region
ellipse(mu0, shape=data.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
# add mean 
points(data.mean[1], data.mean[2], pch = 16, col = 'red', cex=1.5)
# confidence interval
ellipse(data.mean, shape=data.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, center.pch = 16)
# Sim-CI 
rect(T2[1,1],T2[2,1],T2[1,3],T2[2,3], border='red', lwd=2)


# if p>2 we can plot the Sim-CI as:
matplot(1:p,1:p,pch='',ylim=range(data),
        xlab='Variables',ylab='T2 for a component', 
        main='Simultaneous T2 conf. int. for the components')
for(i in 1:p) segments(i,T2[i,1],i,T2[i,3],lwd=3,col=i)
points(1:p, T2[,2], pch=16, col=1:p)
# Is mu0 inside the rectangular region? We add it to the plot
points(1:p, mu0, lwd=3, col='orange')




# Sim-CI on the worst direction: 
# direction along which the T2 statistics (univariate) is maximized (from Maximum Lemma)

worst <- data.invcov %*% (data.mean-mu0)
worst <- worst/sqrt(sum(worst^2))         # normalization of the vector to have a direction
theta.worst <- atan(worst[2]/worst[1])+pi # angle with the x-axis

# Confidence Interval along this worst direction, and compare it with mu0: 
IC.worst  <- c( data %*% worst - sqrt(cfr.fisher*(t(worst)%*%D.cov%*%worst)/n),
                data %*% worst,
                data %*% worst + sqrt(cfr.fisher*(t(worst)%*%D.cov%*%worst)/n) )
IC.worst
# mu0%*%worst     # projection on mu0 on the worst direction
(IC.worst[1] < mu0%*%worst) & (mu0%*%worst < IC.worst[2])  
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is inside the Confidence Region).
# False: mu0 is outside the Confidence Region so there is statistical evidence to reject H0 at level alpha


# if p=2 you can plot it
plot(data, asp=1, pch=1, main='scatterplot of data',ylim=range(data))
ellipse(center=data.mean, shape=data.cov/n, radius=sqrt(cfr.fisher), lwd=2, col='red')
abline(v = T2[1,1], col='red', lwd=1, lty=2)
abline(v = T2[1,3], col='red', lwd=1, lty=2)
abline(h = T2[2,1], col='red', lwd=1, lty=2)
abline(h = T2[2,3], col='red', lwd=1, lty=2)
# add mu0
points(mu0[1], mu0[2], pch=16, col='blue', cex=1.5)
# Extremes of IC.worst in the coordinate system (x,y):
x.min <- IC.worst[1]*worst
x.max <- IC.worst[3]*worst
m1.ort <- -worst[1]/worst[2]
q.min.ort <- x.min[2] - m1.ort*x.min[1]
q.max.ort <- x.max[2] - m1.ort*x.max[1]
abline(q.min.ort, m1.ort, col='forestgreen', lty=2,lwd=1)
abline(q.max.ort, m1.ort, col='forestgreen', lty=2,lwd=1)
m1=worst[2]/worst[1] # worst direction
abline(0, m1, col='grey35')
segments(x.min[1],x.min[2],x.max[1],x.max[2],lty=1,lwd=2, col='forestgreen')




# Sim-CI on all the directions:
data.matrix <- as.matrix(data)
theta    <- seq(0, pi - pi/180, by = pi/180)
T2.d     <- NULL
Centerf  <- NULL
Maxf     <- NULL
Minf     <- NULL
for(i in 1:length(theta)) {
  a   <- c(cos(theta[i]), sin(theta[i])) # direction of the current projection
  proj <- mu0%*%a         # projection of mu on the current direction
  mu0_a <- c(mu0_a, proj) # collection of all the projections
  t2  <- (mean(data.matrix %*% a) - (mu0 %*% a) )^2 / ( var(data.matrix %*% a) / n )
  T2.d  <- c(T2.d, t2)
  centerf  <- data.mean %*% a
  maxf     <- data.mean %*% a + sqrt( t(a) %*% data.cov%*% a / n) * sqrt(cfr.fisher)
  minf     <- data.mean %*% a - sqrt( t(a) %*% data.cov%*% a / n) * sqrt(cfr.fisher)
  Centerf  <- c(Centerf, centerf)
  Maxf     <- c(Maxf, maxf)
  Minf     <- c(Minf, minf)
}

# plot the Sim-CI on all the direction
plot(theta, Centerf, main = 'Simultaneous T2 confidence intervals', ylim = range(data), col = 'grey25', type='l',ylab='IC')
for(i in 1:length(theta)) {
  lines(c(theta[i], theta[i]), c(Minf[i], Maxf[i]), col = 'grey75')
  }
lines(c(theta[1], theta[1]), c(Minf[1], Maxf[1]), col = 'red', lwd=2) 
lines(c(theta[91], theta[91]), c(Minf[91], Maxf[91]), col = 'red', lwd=2)
lines(c(theta[which.max(T2.d)], theta[which.max(T2.d)]), c(Minf[which.max(T2.d)], Maxf[which.max(T2.d)]), col = 'forestgreen', lwd=2)
lines(theta, mu_a)
abline(h=mu0, col='black')
lines(theta, Minf, col = 'red', lty = 2)
lines(theta, Maxf, col = 'red', lty = 2)





#### TEST FOR THE MEAN - SIMULTANEOUS CI with BONFERRONI CORRECTION ####

# As before, you need Gaussianity assumption on your data:
mcshapiro.test(data)


n <- dim(data)[1]
p <- dim(data)[2]
data.mean <- sapply(data, mean)  # mean
data.cov <- cov(data)            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix

mu0 <- c(, , , ...) # ******************************************************************


# Bonferroni CI for the MEAN at level (1-alpha):
k <- p # number of intervals I want to compute (set in advance)
alpha <- # *****************************************************
cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = data.mean - cfr.t*sqrt(diag(data.cov)/n),
            center = data.mean, 
            sup = data.mean + cfr.t*sqrt(diag(data.cov)/n))
Bf
# if mu0 is contained in each interval, we cannot reject H0. 
# if mu0 is NOT contained in each interval, there is evidence to reject H0. 

# Bonferroni CI for the VARIANCE at level (1-alpha)
k <- p
BFvar <- cbind(inf = ((n1-1)*diag(data.cov))/qchisq(1-alpha/(2*k), n1-1), 
               center =((((n1-1)*diag(data.cov))/qchisq(alpha/(2*k), n1-1)+(n1-1)*diag(data.cov)/qchisq( 1-alpha/(2*k),n1-1)))/2, 
               sup =(n1-1)*diag(data.cov)/qchisq( alpha/(2*k),n1-1))


# if p=2, you can plot the Sim-CI, the Bonferroni intervals and the confidence region:
plot(data, asp = 1,main='Confidence and rejection regions')
# rejection region
ellipse(mu0, shape=data.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
# add mean
points(data.mean[1], data.mean[2], pch = 16, col = 'red', cex=1.5)
# confidence region
ellipse(data.mean, shape=data.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, center.pch = 16)
# Sim-CI
rect(T2[1,1],T2[2,1],T2[1,3],T2[2,3], border='red', lwd=2)
# Bonferroni intervals
rect(Bf[1,1],Bf[2,1],Bf[1,3],Bf[2,3], border='orange', lwd=2)
legend('topleft', c('Rej. Reg.', 'Conf. Reg','T2-sim', 'Bonferroni'),
       col=c('blue','red','red','orange'), lty=c(2,2,1,1), lwd=1, cex=0.7)


# if p>2 we can plot the Sim-CI with Bonferroni correction as:
matplot(1:k,1:k,pch='',ylim=range(data),
        xlab='Variables',ylab='T2 for a component', 
        main='Simultaneous T2 conf. int. for the components')
for(i in 1:k) segments(i,T2[i,1],i,T2[i,3],lwd=3,col=i) # Sim-CI
points(1:k, T2[,2], pch=16, col=1:k)
for(i in 1:k) segments(i,Bf[i,1],i,Bf[i,3],lwd=2,col=i) # Bonferroni
points(1:k, Bf[,2], pch=16, col=1:k)
points(1:k, Bf[,1], pch='-', col=1:k)
points(1:k, Bf[,3], pch='-', col=1:k)
# Is mu0 inside the rectangular region? We add it to the plot
points(1:k, mu0, lwd=3, col='orange')



#### TEST FOR THE MEAN - PAIRED MULTIVARIATE GAUSSIAN, CI ####

# We want to perform this test:
# \[ H_0: \delta = \delta_0 \quad vs\quad H_1: \delta \neq \delta_0 \]
# with $\delta_0=c(0,0)$
# where $\delta$ is the mean of the difference sample.
# Note that we need Gaussian assumptions on this sample.

n <- dim(data)[1]  
p <- dim(data)[2]  
# sample of differences (lab1-lab2 for each variable)
data1 <- data[,c( which(colnames(data)=='...') ,which(colnames(data)=='...') , , ,)] # ******************************************************************************
data2 <- data[,c(, , , ,)] # ******************************************************************************
data3 <- data[,c(, , , ,)] # ******************************************************************************

D <- data.frame(cbind(data1[,1:p]-data2[,1:p])) # imposta qui di quali data vuoi fare la differenza ***************************************************
colnames(D) <- paste('DV', 1:p, sep='')


# Checking Gaussianity assumption of sample of differences: 
# we are not interested in the Gaussianity of the original data.
mcshapiro.test(D)$pvalue


#Test on linear combinations
#Se sono su dataset diversi unisco i dataset!

# mu0 <- c(... ,... )           # di solito mu0<-0 **********************************************************************
# a <- c(..., ..., ...)         # dimensione = dimensione dataset, per le colonne che non considero metto 0 #******************  
# T <- (data.mean%*%a - mu0%*%a)/sqrt((t(a)%*%data.cov%*%a)/n) #mu0%*%a secondo me non c'è
# T
# P <- (1 - pt(abs(T), n-1))*2 #IF BILATERAL!!!!!!!!!!!!!
# P   #Check if P is higher or smaller that the alpha (1-level) given
# 

#Unilateral:
#H0 <= :  P <- (1 - pt(T, n-1))
#H0 >= :  P <- pt(T, n-1)
# TEST UNILATERALE!!!!
# T <- (data.mean%*%a)/sqrt((t(a)%*%data.cov%*%a)/n)
# 
# P <- (pt(T, n-1))
# P   #Check if P is higher or smaller that the alpha (1-level) given
# 
# 

#CASO UNIVARIATO: vedi qualche riga sotto 


n <- dim(D)[1]  
p <- dim(D)[2]  
D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)
alpha   <- ...   # ******************************************************************************
delta.0 <- c(..., ...) # ******************************************************************************


# Compute the T2 statistics:
D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p) # p-value that defines the rejection region

# Check if mu0 is outside the rejection region {D.T2>cfr.fisher}:
ifelse(D.T2 < cfr.fisher,
       print("TRUE: there is no statistical evidence to reject H0"), 
       print("FALSE: there is statistical evidence to reject H0"))
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# compute the pvalue 
pvalue <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
pvalue
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 


#CASO UNIVARIATO

#OPPURE USO (Solo nel caso in cui la differenza è univariata):
#SE NON HO LA DIFFERENZA DEVO MODIFICARE DATA1 E DATA2 MOLTIPLICANDO DATA2 per -a2!! (RICORDATI IL -)!
data1 <- data[,c(which(colnames(data)=='...'))] # ******************************************************************************
data2 <- data[,c(which(colnames(data)=='...'))] # ******************************************************************************

data1 <- as.data.frame(data1)
data2 <- as.data.frame(data2)

n <- dim(data1)[1]  
p <- dim(data1)[2] 

D <- data.frame(cbind(data1[,1:p]- data2[,1:p])) # scegli espressione, pu� essere per esempio: D <- data.frame(cbind(data1[,1:p]- 1.2 * data2[,1:p])) ******************************************************************************
colnames(D) <- paste('DV', 1:p, sep='')
D

# Checking Gaussianity before performing t test
mcshapiro.test(D)$pvalue

t.test(D, mu=0, alternative="two.sided", paired=TRUE,conf.level = 0.9)  #Mettere "greater" se ho H0 <= , "l" se HO>=


# SE CHIEDE ELLIPTICLA REGION THAT CONTAINS tot% of the data go to the chapter GAUSSIANITY


# if p=2 we can plot the ellipsoidal region
plot(D, asp=1, pch=1, main='Dataset of the Differences',ylim=range(D))
# Ellipsoidal confidence region with confidence level 1-alpha
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt((n-1)*p/(n-p)*qf(1-alpha,p,n-p)),
        col='red', lwd=2)
# add sample mean
points(D.mean[1], D.mean[2], pch=16, col='red', cex=1.5)
# Ellipsoidal Rejection Region of level 1-alpha
ellipse(center=delta.0, shape=D.cov/n, radius=sqrt((n-1)*p/(n-p)*qf(1-alpha,p,n-p)),
        col='blue', lwd=2)
# add delta.0
points(delta.0[1], delta.0[2], pch=16, col='blue', cex=1.5)




# Simultaneous T2 confidence intervals on the coordinate directions:
T2 <- cbind(inf = D.mean - sqrt(cfr.fisher*diag(D.cov)/n),
            center = D.mean , 
            sup = D.mean + sqrt(cfr.fisher*diag(D.cov)/n))
T2


# Bonferroni CI for the MEAN at level (1-alpha):
k <- p # number of intervals I want to compute (set in advance)
alpha <- # *****************************************************
  cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = D.mean - cfr.t*sqrt(diag(D.cov)/n),
            center = D.mean, 
            sup = D.mean + cfr.t*sqrt(diag(D.cov)/n))
Bf

# AUTOMATIZZATO



#### TEST FOR THE MEAN - PAIRED MULTIVARIATE GAUSSIAN, Sim-CI ####

# sample of differences (lab1-lab2 for each variable)
data1 <- data[,c(, , , ,)] # ******************************************************************************
data2 <- data[,c(, , , ,)] # ******************************************************************************
data3 <- data[,c(, , , ,)] # ******************************************************************************

D <- data.frame(cbind(data1[,1:p]-data2[,1:p])) # imposta qui di quali data vuoi fare la differenza ***************************************************
colnames(D) <- paste('DV', 1:p, sep='')


# Checking Gaussianity assumption of sample of differences: 
# we are not interested in the Gaussianity of the original data.
mcshapiro.test(D)


data0 <- data # save original data
data <- D     # now data is the sample of the differences!

# see the chapter ## TEST FOR THE MEAN - SIMULTANEOUS CI ##











#### TEST FOR THE MEAN - PAIRED MULTIVARIATE GAUSSIAN, Sim-CI with Bonferroni ####


# sample of differences (lab1-lab2 for each variable)
data1 <- data[,c(, , , ,)] # ******************************************************************************
data2 <- data[,c(, , , ,)] # ******************************************************************************
data3 <- data[,c(, , , ,)] # ******************************************************************************

D <- data.frame(cbind(data1[,1:p]-data2[,1:p])) # imposta qui di quali data vuoi fare la differenza ***************************************************
colnames(D) <- paste('DV', 1:p, sep='')


# Checking Gaussianity assumption of sample of differences: 
# we are not interested in the Gaussianity of the original data.
mcshapiro.test(D)


data0 <- data # save original data
data <- D     # now data is the sample of the differences!

# see the chapter ## TEST FOR THE MEAN - SIMULTANEOUS CI with BONFERRONI CORRECTION ##


#### TEST FOR THE MEAN - REPEATED MEASURES ####

# First representation of our data:
matplot(t(data))


n <- dim(data)[1]
q <- dim(data)[2]

# We need Gaussianity assumption on the increments: I perform mcshapiro.test()
# on original data:
mcshapiro.test(data)
# se abbiamo i dati gaussiani anche la loro combinazione lineare è gaussiana

# We build the contrast matrix: # ******************************************************************************
# differences with baseline
C <- matrix(c(-1, 1, 0, 0, 
              -1, 0, 1, 0,
              -1, 0, 0, 1), q-1, q, byrow=T)
# consecutive differences 
C <- matrix(c(-1, 1, 0, 0, 
              0, -1, 1, 0,
              0, 0, -1, 1), q-1, q, byrow=T)

# We want to perform the following test at level $\alpha$ : # ******************************************************************************
# \[ H_0: C \mu = \delta_0 \quad H_1:C \mu \neq \delta_0 \]
# where $\delta_0 = c(0,0,0,0)$ # ******************************************************************************

# Compute mean and covariance matrix:
alpha = 0.05 # ******************************************************************************
delta.0 <- c(0,0,0) #dim=q-1 ******************************************************************************
M <- sapply(data, mean)
S <- cov(data)
Md <- C %*% M            # mean of transformed data (we apply C to the matrix of the mean M)
Sd <- C %*% S %*% t(C)   # variance/covariance matrix of transformed data
Sdinv <- solve(Sd)

# compute the test statistics and the radius of the ellipsoid:
T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 ) 
cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1))

# Check if mu0 is outside the rejection region {T2>cfr.fisher}:
ifelse(T2 < cfr.fisher,
       "TRUE: there is no statistical evidence to reject H0", 
       "FALSE: there is statistical evidence to reject H0")
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# compute the pvalue:
pvalue <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
pvalue 
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 



## SIMULTANEOUS CI:
# Simultaneous T2 intervals to check each difference simultaneously:
IC.T2 <- cbind( Md - sqrt(cfr.fisher*diag(Sd)/n) , Md, Md + sqrt(cfr.fisher*diag(Sd)/n) ) 
IC.T2
# check if the intervals contain the components of delta0.
k <- q - 1
## Graphical representation of the intervals:
matplot(matrix(1:3,k,3, byrow = T), t(IC.T2), type='b', pch='',xlim=c(0,4),xlab='', ylab='', main='Confidence intervals')
segments(matrix(1:k,k,1), IC.T2[,1], matrix(1:k,k,1),IC.T2[,3], col='orange', lwd=2) 
points(1:k, IC.T2[,2], col='orange', pch=16)
points(1:k+.05, delta.0, col='black', pch=16) 


# Confidence intervals of global level 1-alpha% for the daily increments
k <- q-1
ICmean <- cbind(Md - sqrt(diag(Sd)/n) * qt(1 - alpha/(2*k), n-1),
                Md,
                Md + sqrt(diag(Sd)/n) * qt(1 - alpha/(2*k), n-1))
ICmean
# comment 
# for (i in 1:(q-1))
# print(paste('Reject H0 in direction ',i,': ', !(delta.0[i]>ICmean[i,1] & delta.0[i]<ICmean[i,3]),sep=''))


## BONFERRONI CI: (differenze)
k <- q - 1 # number of increments (i.e., dim(C)[1]) 
cfr.t <- qt(1-alpha/(2*k), n-1)
IC.BF <- cbind( Md - cfr.t*sqrt(diag(Sd)/n) , Md, Md + cfr.t*sqrt(diag(Sd)/n) ) 
IC.BF
# check if the intervals contain the components of delta0.

## Graphical representation of the intervals:
matplot(matrix(1:3,k,3, byrow = T), t(IC.BF), type='b', pch='',xlim=c(0,4),xlab='', ylab='', main='Confidence intervals')
segments(matrix(1:k,k,1), IC.BF[,1], matrix(1:k,k,1),IC.BF[,3], col='orange', lwd=2) 
points(1:k, IC.BF[,2], col='orange', pch=16)
points(1:k+.05, delta.0, col='black', pch=16) 
#Se voglio anche i Simultaneous
# segments(matrix(1:k+.1,k,1),IC.T2[,1],matrix(1:k+.1,k,1),IC.T2[,3], col='blue', lwd=2) 
# points(1:k+.1,IC.T2[,2], col='blue', pch=16)   #T2= Simultaneous

#??? tde 10-07-18
#IC.T2 <- cbind( Md - sqrt(cfr.fisher*diag(Sd)/n) , Md, Md + sqrt(cfr.fisher*diag(Sd)/n) ) 
#IC.T2


## Se chiede solo dei bonferroni sulle variabili andare al capitolo degli intervalli simultaneio con
## bonferroni correction




#### TEST FOR THE MEAN - ONE AT THE TIME

n <- dim(data)[1]
p <- 1
data.mean <- mean('colonna di data che si vuole considerare')  # mean
data.cov <- var'colonna di data che si vuole considerare')            # var/cov matrix
data.invcov <- solve(data.cov)   # inverse of var/cov matrix



# Bonferroni CI for the mean at level (1-alpha):
k <- 1 # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = data.mean - cfr.t*sqrt(data.cov/n),
            center = data.mean, 
            sup = data.mean + cfr.t*sqrt(data.cov/n))
Bf



#### TEST FOR THE MEAN - INDEPENDENT GAUSSIAN POPULATIONS ####

# We want to perform this test at level $\alpha = 0.05$: # ******************************************************************************
# \[ H_0: \mu_1 = \mu_2 \quad H_1: \mu_1 \neq \mu_2 \]
# namely: 
# \[ H_0: \mu_1 - \mu_2 = c(0,0) \quad H_1: \mu_1 - \mu_2 \neq c(0,0) \]

n1 <- dim(data1)[1] # statistical units of the first group
n2 <- dim(data2)[1] # statistical units of the second group
p  <- dim(data1)[2] 

# sample mean, covariance matrices and matrix Spooled:
data1.mean <- sapply(data1, mean) 
data2.mean <- sapply(data2, mean) 
data1.cov <- cov(data1)
data2.cov <- cov(data2)
Sp <- ((n1-1)*data1.cov + (n2-1)*data2.cov)/(n1+n2-2) # Spooled matrix 
Spinv <- solve(Sp)


# check assumption of the test:

# 1. Gaussianity of each population
mcshapiro.test(data1)
mcshapiro.test(data2)
# High (> 0.1): there is no statistical evidence to reject H0 (I can assume Gaussianity of data)
# Very low (< 0.05): there is statistical evidence to reject H0 (I can NOT assume Gaussianity of data)
#
# 2. Homogeneity of Covariances
cov1 <- cov(data1)
cov2 <- cov(data2)
#  I need similar covariances matrix
image(cov1)
image(cov2)

# perform the test (of level 1- alpha) (H0: data1.mean-data2.mean = delta0 VS H1: data1.mean-data2.mean != delta0)
alpha <- 0.05 # ******************************************************************************
delta.0 <- c(0,0, ...) #******************************************************************************
T2 <- n1*n2/(n1+n2) * (data1.mean-data2.mean-delta.0) %*% Spinv %*% (data1.mean-data2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)

# Check if mu0 is outside the rejection region {T2>cfr.fisher}:
ifelse(T2 < cfr.fisher,
       "TRUE: there is no statistical evidence to reject H0", 
       "FALSE: there is statistical evidence to reject H0")
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha


# compute pvalue:
pvalue <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
pvalue
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 


## CONFIDENCE REGION FOR THE DIFFERENCE IN MEAN OF THE COMPONENTS (RELATIVE)
# Characterize the ellipse:
# Directions of the axes
eigen(Sp)$vector

# Radius
r <- sqrt(cfr.fisher)

# Length of the semi-axes
r*sqrt(eigen(Sp)$values*(1/n1+1/n2))

par(mfrow=c(1,2))
plot(data1 , col=rainbow(2)[1], asp=1, pch=16, main='Original data and groups')
points(data2, col=rainbow(2)[2],asp=1, pch=16)

xlimit <- c((data1.mean-data2.mean-sqrt(cfr.fisher))[1], (data1.mean-data2.mean+sqrt(cfr.fisher))[1])
ylimit <- c((data1.mean-data2.mean-sqrt(cfr.fisher))[2], (data1.mean-data2.mean+sqrt(cfr.fisher))[2])

par(mfrow=c(1,2))
plot(satellite , col=sata+1, asp=1, pch=16, main='Original data and groups')
plot(satellite, xlim=xlimit, ylim=ylimit, pch='', asp=1, 
     main='Elliptic region for the mean diff. (data1 - data2)')
# confidence region and sample mean in blue
ellipse(center=data1.mean-data2.mean, shape=Sp*(1/n1+1/n2), radius=sqrt(cfr.fisher), 
        lwd=2, col='blue')



## SIMULTANEOUS T2 CI for the difference in mean (of level 1- alpha):
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
IC.T2 <- NULL
for (i in 1:p) {
  IC.T2.temp <- c(data1.mean[i]-data2.mean[i]-sqrt(cfr.fisher*Sp[i,i]*(1/n1+1/n2)), data1.mean[i]-data2.mean[i]+sqrt(cfr.fisher*Sp[i,i]*(1/n1+1/n2)) )
  IC.T2 <- rbind(IC.T2, IC.T2.temp)
}
dimnames(IC.T2)[[2]] <- c('inf','sup')     
IC.T2
# check if the CI's contain delta.0


## BONFERRONI CI (of level 1-alpha) for the difference of each component
k <- p
alpha <- alpha/k
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
IC.T2 <- NULL
for (i in 1:p) {
  IC.T2.temp <- c(data1.mean[i]-data2.mean[i]-sqrt(cfr.fisher*Sp[i,i]*(1/n1+1/n2)), data1.mean[i]-data2.mean[i]+sqrt(cfr.fisher*Sp[i,i]*(1/n1+1/n2)) )
  IC.T2 <- rbind(IC.T2, IC.T2.temp)
}
dimnames(IC.T2)[[2]] <- c('inf','sup')     
IC.T2
# check if the Bonf-CI's contain delta.0







#### TEST FOR THE MEAN - INDEPENDENT BERNOULLI POPULATIONS - one-at-the-time ####

# Suppose you have two Independent Bernoulli Populations 
# $X_1 \sim Be(\mu_1), X_2 \sim Be(\mu_2)$. 

# We want to perform this test at level $alpha=0.05$
# \[ H_{0i}: \mu_{1i} = \mu_{2i} \quad vs \quad H_{1i}: \mu_{1i} \neq \mu_2i \quad \quad \forall i \in 1,\dots,p\]

p <- dim(data)[2]

data1 <- data[,c(, , , , )] # ******************************************************************************
data2 <- data[,c(, , , , )] # ******************************************************************************

n1 <- dim(data1)[1]
n2 <- dim(data2)[1]

# compute the sample mean of the two populations:
x.mean1 <- sapply(data1, mean) 
x.mean2 <- sapply(data2, mean)

# set alpha (significance level for each of the test)
alpha <- 0.05 # ******************************************************************************

p.hat <- (x.mean1*n1+x.mean2*n2)/(n1+n2) 
x.var <- (p.hat*(1-p.hat))

z.i <- (x.mean1-x.mean2)/sqrt(x.var*(1/n1+1/n2))
p.i <- ifelse(z.i<0, 2*pnorm(z.i),2*(1-pnorm(z.i))) 

# Check if alpha is greater than the pvalue
for (i in 1:p) {
  ifelse(p.i < alpha,
         "TRUE: there is no statistical evidence to reject H0", 
         "FALSE: there is statistical evidence to reject H0")
}
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# for which components I cannot reject the null hypothesis:
which(p.i<alpha) 



#### TEST FOR THE MEAN - INDEPENDENT BERNOULLI POPULATIONS - Simultaneous Bonferroni CI####

# Suppose you have two Independent Bernoulli Populations 
# $X_1 \sim Be(\mu_1), X_2 \sim Be(\mu_2)$. 

# We want to perform this test at level $alpha=0.05$
# \[ H_{0i}: \mu_{\i} = \mu_{2i} \quad vs \quad H_{1i}: \mu_{1i} \neq \mu_2i \quad \quad \forall i \in 1,\dots,p\]

p <- dim(data)[2]
data1 <- data[,c(, , , , )] # ******************************************************************************
data2 <- data[,c(, , , , )] # ******************************************************************************
k <- p # ******************************************************************************

n1 <- dim(data1)[1]
n2 <- dim(data2)[1]

# compute the sample mean of the two populations:
x.mean1 <- sapply(data1, mean) 
x.mean2 <- sapply(data2, mean)

# set alpha (significance level for each of the test)
alpha <- 0.05 # ******************************************************************************
alpha <- alpha/k 

p.hat <- (x.mean1*n1+x.mean2*n2)/(n1+n2) 
x.var <- (p.hat*(1-p.hat))

z.i <- (x.mean1-x.mean2)/sqrt(x.var*(1/n1+1/n2))
p.i <- ifelse(z.i<0, 2*pnorm(z.i),2*(1-pnorm(z.i))) 

# Check if alpha is greater than the pvalue
for (i in 1:p) {
  ifelse(p.i < alpha,
         "TRUE: there is no statistical evidence to reject H0", 
         "FALSE: there is statistical evidence to reject H0")
}
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha

# for which components I cannot reject the null hypothesis:
which(p.i<alpha) 


# if you have already computed the one-at-the-time CI, you can simply adjust the pvalues by:
p.Bf <- p.adjust(p.i, method = 'bonferroni') 






























#### (M)ANOVA scheme: ####

# p = number of characteristics (X)
# g = number of different types of factor.1
# b = number of different types of factor.2 (if it exists)

# ANOVA:      p = 1
# MANOVA:     p > 1

# One-Way: 	  only factor.1
# Two-Ways:   factor.1 and factor.2

#### One-Way ANOVA ####
# L'analisi della varianza a una via è un metodo statistico per testare 
# l'ipotesi nulla (H0) in base alla quale le medie di tre o più popolazioni 
# sarebbero uguali, contro l'ipotesi alternativa (Ha)



p <- 1
# setting dataset:
# labels:
factor1 <- factor(data$......) # ******************************************************************************
# var numerica:
x <- data$.....        # ******************************************************************************

# boxplot of data divided by labels:
plot(factor1, x, xlab='labels', ylab='x', col='grey85', main='Dataset')
# look at if there are some differences in terms of variability between the different groups. 

# set important quantities:
n <- dim(data)[1]         # number of observations
ng <- table(factor1)      # number of observations in each group
treat <- levels(factor1)  # levels of the treatment
g <- length(treat)        # number of levels/groups/labels


# We are building this model:
# \[ x_{ij} = \mu + \tau_i + \varepsilon_{ij} \quad \quad \varepsilon_{ij} \sim N(0, \sigma^2) \]
# where
# $\mu$ : overall mean of the x;
# $\tau_i$: effect of the treatment $i$;
# $\varepsilon_{ij}$: additive Gaussian random noise.
# 
# We want to perform this test to see if the treatment given by the labels has an effect on x:
# \[ H_0: \tau_1=\tau_2=\dots=\tau_g=0 \quad vs \quad H_1: (H_0)^C \]
# namely:
# $H_0$: the treatment has no effect;
# $H_1$: at least one treatment has an effect;
# 
# Model assumptions:
# - Gaussian distribution of the error;
# - Homoschedasticity.


# Verify assumption of the model:
# 
# - Normality in each group:
# 
# we are in ANOVA setting, so we perform $g$ shapiro tests, one for each group:
pvalue <- NULL
for (i in 1:g) {
  pval <- shapiro.test(x[factor1==treat[i]])$p
  pvalue <- c(pvalue, pval)
}
pvalue
# If pvalues are large, I can accept the hypothesis of Gaussianity of data.

# - same covariance structure
#
# I can perform the Bartlett test (that relies on Gaussianity assumption) to check homogeneity of variances. 
# Namely, the test I'm performing is the following:
# \[ H_0: \sigma_1 = \sigma_2 = \dots = \sigma_g \quad vs \quad H_1: \exists i,j s.t. \sigma_i \neq \sigma_j\]

bartlett.test(x, factor1)
# if pvalue is high, I don't have enough evidence to reject the null hypothesis 
# of homoschedasticity of data, so I can assume the homogeneity of variances.
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, I can NOT assume Homoschedasticity
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Homoschedasticity


# Now I can perform the One-Way ANOVA:
fit <- aov(x ~ factor1)     # aov( variable of interest ~ treatment )
summary(fit)

# NOTE: HOW TO READ THE SUMMARY:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ AOV summary ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#              Df   Sum Sq      Mean Sq      F value     Pr(>F)
#  treat      (g-1) SStreat  SStreat/(g-1)  Fstatistic  p-value [H0: tau.i=0 for every i]
#  Residuals  (n-g) SSres     SSres/(n-g)
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ##
#
# SStreat: component of the variance, between variability (g-1 degrees of freedom).
# SSres: component of the variance, within variability (n-g degrees of freedom).
# Fstat: SStreat*(n-g) / SSres*(g-1)
# To see if the treatment has an effect on x you have to look at the pvalue of the test Pr(>F)
#
# if pvalue is small: we reject H0, so we have evidence to say that the treatment has an effect on x;
# if pvalue is large: we cannot reject H0, so we don't have evidence to say that the treatment has an effect on x;


#STIME:
# Estimate variances
W <- sum(fit$residuals^2)  # SS_res
var <- W/(n-g)     # SS_res/gdl(res)   
var
# Estimate the great mean mu:
m <- mean(data[,1])
m
# Estimate tau.i:
tau1  <- mean(data[factor1=='GRUPPO1',1]) - m  # tau.1 #****************************************
tau2  <- mean(data[factor1=='GRUPPO2',1]) - m  # tau.2 #****************************************
# tau3  <- mean(data[factor1=='GRUPPO3',1]) - m  # tau.3 #solo se hai 3 gruppi nel factor ****************************************
tau <- cbind(tau1,tau2,tau3)
tau 
# point-wise estimate of the mean:
m1 <- m + tau1
m2  <- m + tau2
# m3  <- m + tau3
mu <- cbind(m1,m2,m3)
mu 



# Using BONFERRONI's INEQUALITY estimate through BILATERAL confidence
# intervals (with global confidence 95%) the means and variances of the
# subpopulations associated with the chosen model.

N <- dim(data)[1]


### length(levels(factor1)) NON FUNZIONA lab 7
g <- length(levels(factor1)) # ************** inserire factor1 se si parte da una one way o il factor
# rimasto dal modello ridotto **************************************
DF <- N-g

alpha <- 0.05
k <- g+1 # il +1 si riferisce a quando c'è da fare CI per la varianza

qT <- qt(1-alpha/(2*k), DF)
qCinf <- qchisq(1 - alpha / (2*k), DF)
qCsup <- qchisq(alpha / (2*k), DF)

Spooled <- (t(fit$res) %*% fit$res)/DF  
Spooled


# Tre gruppi
m1 <- mean(data[which(factor1==levels(factor1)[1]),1])
m2 <- mean(data[which(factor1==levels(factor1)[2]),1])
m3 <- mean(data[which(factor1==levels(factor1)[3]),1])
medie <- c(m1,m2,m3)


ng <- c(length(which(factor1==levels(factor1)[1])),length(which(factor1==levels(factor1)[2])),length(which(factor1==levels(factor1)[3])))

BF    <- rbind(cbind(inf=medie - sqrt(as.vector(Spooled) / ng) * qT,
                     sup=medie + sqrt(as.vector(Spooled) / ng) * qT),
               c(inf=Spooled * DF / qCinf,
                 sup=Spooled * DF / qCsup))
BF




# Now we want to see which treatment is responsible for this effect. 
# So we perform g*(g-1)/2 tests simultaneously, one for each couple of treatments.
# We use BONFERRONI approach.

k <- g*(g-1)/2    # +1 se chiede sia media che varianza number of comparisons
alpha <- 0.05     # overall level # ******************************************************************************

Mediag <- tapply(x, factor1, mean) 
SSres <- sum(residuals(fit)^2)
S <- SSres/(n-g)





# CONFIDENCE INTERVALS for all the differences in mean at level 1-alpha - BONFERRONI
ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * 
                         sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * 
                         sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * 
                                         sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * 
                                         sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
  }}

# plot CI for all the differences in mean - BONFERRONI
par(mfrow=c(1,2))
plot(factor1, x, xlab='treatment', ylab='x', col = rainbow(g), las=2)

h <- 1
plot(c(1,g*(g-1)/2),range(ICrange), pch='',
     xlab='pairs treat', ylab='Conf. Int. tau x')
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    ind <- (i-1)*g-i*(i-1)/2+(j-i)
    lines (c(h,h), c(ICrange[ind,1],ICrange[ind,2]), col='grey55'); 
    points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
    points(h, ICrange[ind,1], col=rainbow(g)[j], pch=16); 
    points(h, ICrange[ind,2], col=rainbow(g)[i], pch=16); 
    h <- h+1
  }}
abline(h=0)
# Note: if the interval do not contains 0, 
# there is a strong statistical difference between the effects of the two treatments.



#ANOVA BONFERRONI PER LA MEDIA nel caso 1-way:   (k = #gruppi)
k <- g
S <- sum(residuals(fit)^2)/(n-g)
                                                   
alpha<- ... #(1-livello!)    # overall level #************************************************************************
                                                   
M1  <- mean(x[data$GRUPPO=='...'])# cambia GRUPPO************************************************************************
M2  <- mean(x[data$GRUPPO=='...'])# cambia GRUPPO ************************************************************************
M3  <- mean(x[data$GRUPPO=='...'])# cambia GRUPPO ************************************************************************
#FINO Al g-esimo
label <- levels(factor1)
n1 <- length(x[data$GRUPPO=='...']) #cambia GRUPPO************************************************************************
n2 <- length(x[data$GRUPPO=='...']) #cambia GRUPPO************************************************************************
n3 <- length(x[data$GRUPPO=='...']) #cambia GRUPPO************************************************************************
#FINO Al g-esimo
t <- qt(1-alpha/(2*k),n-g)
                                                   
                                                   
# Conf int for the means
ICB1<-data.frame(L=M1-sqrt(S*(1/n1))*t,C=M1,U=M1+sqrt(S/n1)*t)
ICB2<-data.frame(L=M2-sqrt(S*(1/n2))*t,C=M2,U=M2+sqrt(S/n2)*t)
ICB3<-data.frame(L=M3-sqrt(S*(1/n3))*t,C=M3,U=M3+sqrt(S/n3)*t)
ICB<-data.frame(rbind(ICB1,ICB2,ICB3))
ICB




# CONFIDENCE INTERVALS for variances (è sempre la stessa)
chi_u <- qchisq(alpha/(2*k),n-g)
chi_l <- qchisq(1-alpha/(2*k),n-g)
ICBV <- data.frame(L=(n-g)*S/chi_l, C=S,U=(n-g)*S/chi_u)
ICBV




# BONFERRONI AND BENJAMINI CORRECTIONS ON PVALUES
# One-at-the-time CI:
# lower triangular matrix: lower bound of CI (i treat - j treat)
# upper triangular matrix: upper bound of CI (i treat - j treat)
# diagonal is 0 (i treat - i treat = 0)

Auni <- matrix(0,g,g)
for(i in 1:g) {
  for(j in i:g) {
    Auni[i,j] <- Mediag[i]-Mediag[j] + qt(1-alpha/2, n-g) * 
      sqrt( S * ( 1/ng[i] + 1/ng[j] ) )}
  for(j in 1:i) {
    Auni[i,j] <- Mediag[j]-Mediag[i] - qt(1-alpha/2, n-g) * 
      sqrt( S * ( 1/ng[i] + 1/ng[j] ) )}
  Auni[i,i]     <- 0
}
#Plot CI of the differences WITHOUT corrections
x11( width=14, height=7)
par(mfrow=c(1,2))
h <- 1
plot(c(1,g*(g-1)/2),range(Auni), pch='', xlab='pairs treat', 
     ylab='CI delta x', main='Univariate Conf. Int.', col='grey55')

for(i in 1:g) {
  for(j in (i+1):g) {lines (c(h,h), c(Auni[i,j],Auni[j,i])); 
    points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
    points(h, Auni[i,j], col=rainbow(g)[i], pch=16); 
    points(h, Auni[j,i], col=rainbow(g)[j], pch=16); 
    h <- h+1
  }}
abline(h=0)

# One-at-the-time CI pvalue (NO CORRECTIONS): 
P <- matrix(0,g,g)
for(i in 1:g) {
  for(j in i:g) {
    P[i,j] <- (1-pt(abs((Mediag[i]-Mediag[j]) / 
                          sqrt( S * ( 1/ng[i] + 1/ng[j] ) ) ), n-g))*2}
  for(j in 1:i) {
    P[i,j] <- (1-pt(abs((Mediag[i]-Mediag[j]) / 
                          sqrt( S * ( 1/ng[i] + 1/ng[j] ) ) ), n-g))*2}
  P[i,i]     <- 0
}

p <- NULL
for (i in 1:g){
  temp <- P[i,(i+1):g]
  p <- c(p, temp)
}
p # vector of pvalues

# Bonferroni corrections on pvalues:
p.bonf <- p.adjust(p, 'bonf')
# Indexes of the couples for which Bonf correction tells us that there is a 
# significant difference at level alpha=5%:
which(p.bonf<alpha) 

# Benjamini-Hockberg corrections on pvalues:
p.fdr <- p.adjust(p, 'fdr')
# Indexes of the couples for which BH correction tells us that there is a 
# significant difference at level alpha=5%:
which(p.fdr<alpha)

# PLOTTARE I VARI P-VALUES
#
# plot(1:15, p, ylim=c(0,1), type='b', pch=16, col='grey55', xlab='pairs treat',
#      main='P-values')
# abline(h=alpha, lty=2)
# 
# # Bonferroni correction
# p.bonf <- p.adjust(p, 'bonf') 
# lines(1:15, p.bonf, col='blue', pch=16, type='b')
# 
# # Correction according to the false discovery rate (Benjamini-Hockberg)
# p.fdr <- p.adjust(p, 'fdr')
# lines(1:15, p.fdr, col='red', pch=16, type='b')
# 
# legend('topleft', c('Not corr.', 'Bonf.', 'BH'), col=c('grey55', 'blue', 'red'), pch=16)
# 
# which(p.bonf<alpha)
# which(p.fdr<alpha)


#### One-Way MANOVA ####
# In statistica, l'analisi multivariata della varianza (MANOVA) 
# è una procedura per confrontare le medie campionarie multivariate.

# setting dataset:
# labels:
factor1 <- factor(data$...) # ******************************************************************************
# var numerica:
x <- data[, c(1, , , ...)] # ******************************************************************************

p <- dim(x)[2]

# set important variables: 
treat <- levels(factor1)
g <- length(treat)
n <- dim(data)[1]         # number of observations
ng <- table(factor1)      # number of observations in each group

n1=ng[1]
n2=ng[2]
#... #************************************************************************************

## graphical exploration:

# scatterplot

colore <- rep(rainbow(g), ng)
pairs(x, col = colore, pch=16)

#id of measurement with treatment i-th
i1=which(factor1==treat[1])
i2=which(factor1==treat[2])
#***********************************************************************
       
#BoxPlot
par(mfrow=c(1,dim(x)[2]))
for(i in 1:dim(x)[2]){
  boxplot(x[,i]~factor1, main=colnames(x)[i], ylim=c(min(x[,i]),max(x[,i])), col = rainbow(g))
}


# We are building this model:
# \[ x_{ij} = \mu + \tau_i + \varepsilon_{ij} \quad \quad \varepsilon_{ij} \sim N(0, \sigma^2), \quad \x_{ij}, \mu, \tau_i in \mathbb{R}^p \]
# where
# $\mu$ : overall mean of the x;
# $\tau_i$: treatment effect of the $i$-th group;
# $\varepsilon_{ij}$: additive Gaussian random noise.

# We want to perform this test to see if the treatment given by the labels has an effect on x: 
# \[ H_0: \tau_1=\tau_2=\dots=\tau_g=(0, \dots, 0) \quad vs \quad H_1: (H_0)^C \]
# namely: 
# $H_0$: The membership to a label hasn't any significant effect on the mean
###     of $x_{ij}$ (in any direction of $\mathbb{R}^p$) 
# $H_1$: There exists at least one direction in $\mathbb{R}^p$ along which at least two labels
###     have some feature significantly different


# First of all I have to verify the Hypothesis

# 1) normality (multivariate) in each group (g tests)
Ps <- NULL
for (i in 1:g){
  Ps <- c(Ps, mcshapiro.test(x[factor1 == levels(factor1)[i],])$p)
}
Ps
# High (> 0.1): there is no statistical evidence to reject H0 (So I can assume Gaussianity)
# Very low (< 0.05): there is statistical evidence to reject H0 


# 2) same covariance structure (= same covariance matrix Sigma)
S <- cov(x)
S1 <- cov(x[i1,])
S2 <- cov(x[i2,])
#... ************************************************************************

# I can only check qualitatively:
par(mfrow=c(1,g))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
#... image(S3, ...)************************************************************************


# Fitting the model:
fit <- manova(as.matrix(x) ~ factor1)
summary.manova(fit,test="Wilks") # you can choose different types of lambda

summary.manova(fit,test="Wilks")$p

# if pvalue is small: we reject H0, so we have evidence to say that the treatment has an effect on x;
# if pvalue is large: we cannot reject H0, so we don't have evidence to say that the treatment has an effect on x;


#If I want to see which variable is affected by the treatments:
summary.aov(fit)

#Note that this analysis does NOT say:
#a) which group differ
#b) with respect to which variables the groups in (a) differ.




#To see which treatment has an effect on which variables I perform MANY Bonferroni
# Bonferroni
alpha <- 0.05 #************************************************************************************************
k <- p*g*(g-1)/2 # number of possible couples between the g levels
qT <- qt(1-alpha/(2*k), n-g)
W <- summary.manova(fit)$SS$Residuals
m <- sapply(x,mean) # estimates mu
m1 <- sapply(x[i1,],mean) # estimates mu.1=mu+tau.1
m2 <- sapply(x[i2,],mean) # estimates mu.2=mu+tau.2
#m3 <- sapply(x[i3,],mean) # estimates mu.3=mu+tau.3

inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )

inf13 <- m1-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
sup13 <- m1-m3 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )

inf23 <- m2-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
sup23 <- m2-m3 + qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )

CI <- list(Treat1vs2=cbind(inf12, sup12),
           Treat1vs3=cbind(inf13, sup13),
          Treat2vs3=cbind(inf23, sup23))  #****************************************************
CI

#I have to check for which couple if 0 is inside the interval, in this case i can assume
#that the treatment has no effect of the respective variable

#Plotting the CI
par(mfrow=c(2,p))
for(i in 1:p) {
boxplot(x[,i]~factor1, main=colnames(x)[i], ylim=range(x[,i]), col = rainbow(g)[i])
}

mg <- rbind(m1,m2,...)   #***********************************************************************
temp.min <- min(inf12, inf13, inf23, ...)  #****************************************************
temp.max <- min(sup12, sup13, sup23, ...)  #****************************************************


for(k in 1:p){
  plot(c(1,g*(g-1)/2),ylim=c(temp.min, temp.max), xlim=c(1,g), pch='', 
       xlab='pairs treat', ylab=paste('CI tau',k), 
       main=paste('CI tau',colnames(x)[k]))
 
  lines (c(1,1), c(CI[[1]][k,1],CI[[1]][k,2])); 
  points(1, mg[1,k]-mg[2,k], pch=16); 
  points(1, CI[[1]][k,1], col=rainbow(g)[2], pch=16); 
  points(1, CI[[1]][k,2], col=rainbow(g)[1], pch=16);  
  lines (c(2,2), c(CI[[2]][k,1],CI[[2]][k,2])); 
  points(2, mg[1,k]-mg[3,k], pch=16);
  points(2, CI[[2]][k,1], col=rainbow(g)[3], pch=16); 
  points(2, CI[[2]][k,2], col=rainbow(g)[1], pch=16);
  lines (c(3,3), c(CI[[3]][k,1],CI[[3]][k,2])); 
  points(3, mg[2,k]-mg[3,k], pch=16);
  points(3, CI[[3]][k,1], col=rainbow(g)[3], pch=16); 
  points(3, CI[[3]][k,2], col=rainbow(g)[2], pch=16);  
  abline(h=0) 
}





# MANOVA BONFERRONI PER LA MEDIA nel caso 1-way:   (k = #gruppi)
k <- p*g
W <- summary.manova(fit)$SS$Residuals
                                                    
                                                    
alpha<- ... #(1-livello!)    # overall level #************************************************************************
                                                    
                                                    
m1 <- sapply(x[i1,],mean)
m2 <- sapply(x[i2,],mean)
m3 <- sapply(x[i3,],mean)
                                                    
                                                    
#FINO Al g-esimo

label <- levels(factor1)
n1 <- dim(x[i1,])[1]
n2 <- dim(x[i2,])[1]
n3 <- dim(x[i3,])[1]
#FINO Al g-esimo
t <- qt(1-alpha/(2*k),n-g)
                                                    
                                                    
# Conf int for the means
ICB11<-data.frame(L=m1[1]-sqrt(diag(W)[1]*(1/n1))*t,C=m1[1],U=M1+sqrt(diag(W)[1]/n1)*t)
ICB12<-data.frame(L=m1[2]-sqrt(diag(W)[2]*(1/n1))*t,C=m1[2],U=M1+sqrt(diag(W)[2]/n1)*t)
#ICB13...
ICB21<-data.frame(L=m2[1]-sqrt(diag(W)[1]*(1/n2))*t,C=m2[1],U=M1+sqrt(diag(W)[1]/n2)*t)
ICB22<-data.frame(L=m2[2]-sqrt(diag(W)[2]*(1/n2))*t,C=m2[2],U=M1+sqrt(diag(W)[2]/n2)*t)
#ICB23...
ICB<-data.frame(rbind(ICB11, ICB12,ICB21, ICB22 , ...))
ICB
                                            
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
             



# BONFERRONI FOR VARIANCES WITHIN EACH GROUP
chi_u <- qchisq(alpha/(2*k),n-g)
chi_l <- qchisq(1-alpha/(2*k),n-g)
ICBV1 <- data.frame(L=(n-g)*diag(W)[1]/chi_l,C=S,U=(n-g)*diag(W)[1]/chi_u)  #Var covariata 1
ICBV2 <- data.frame(L=(n-g)*diag(W)[2]/chi_l,C=S,U=(n-g)*diag(W)[2]/chi_u)  #Var covariata 2
ICBV <- data.frame(rbind(ICBV1, ICBV2, ...))
ICBV





#### Two-Ways ANOVA ####

p <- 1

# setting dataset:
# labels:
factor1 <- factor(data$......) # ******************************************************************************
factor2 <- factor(data$......) # ******************************************************************************
factor12 <- factor(paste(factor1, factor2, sep='-'))

# var numerica:
x <- data$.....        # ******************************************************************************

g <- length(levels(factor1))   # number of factor.1 
b <- length(levels(factor2))   # number of factor.2 
gb <- length(levels(factor12)) # number of factor.12 
n <- length(x)/(g*b)           # number of data

M         <- mean(x)                     # overall mean
Mfactor1  <- tapply(x, factor1, mean)    # mean for the types of factor1
Mfactor2  <- tapply(x, factor2, mean)    # mean for the types of factor2
Mfactor12 <- tapply(x, factor12, mean)   # mean for the all possible combinations factor1+factor2


# The complete model (with interaction) is: 
#   \[ X_{ijk} = \mu + \tau_i + \beta_j + \gamma_{ij} + \varepsilon_{ijk}; \quad \varepsilon_{ijk} \sim N(0,\sigma^2) \]
# where: $i=1,2,..... \quad (\text{effect factor1}),  \quad \quad j=1,2,..... \quad (\text{effect factor2})$ # ****************************************************************** 



# Check the ASSUMPTIONS of the model:

# 1) Gaussianity of each interaction/combination of factors:

Ps12 <- NULL
for (i in 1:gb){
  Ps12 <- c(Ps12, shapiro.test(x[factor12 == levels(factor12)[i]])$p.value)
}
Ps12



# 2) Homogeneity of variances 
bartlett.test(x, factor12)
pvalue.bartlett <- bartlett.test(x, factor12)$p.value
pvalue.bartlett 
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, I can NOT assume Homoschedasticity
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Homoschedasticity


## TEST for the global significance of the two factors:
SSfactor1 <- sum(n*b*(Mfactor1 - M)^2)            # or from the summary   
SSfactor2  <- sum(n*g*(Mfactor2  - M)^2)          # or from the summary
SSres   <- sum((x - M)^2) - (SSfactor1+SSfactor2)   # or from the summary

Ftot      <- ( (SSfactor1 + SSfactor2) / ((g-1)+(b-1)))/(SSres / (n*g*b-g-b+1))
Ptot      <- 1 - pf(Ftot, (g-1)+(b-1), n*g*b-g-b+1) # attention to the dgf!
Ptot
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, globally the factor effects ARE significant.
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, globally the factor effects are NOT significant.


# Estimate the great mean mu:
m <- mean(x)
# Estimate tau.i, beta.j:
tau1  <- mean(data[factor1=='GRUPPO11',1]) - m  # tau.1
tau2  <- mean(data[factor1=='GRUPPO12',1]) - m  # tau.2
beta1 <- mean(data[factor2=='GRUPPO21',1]) - m  # beta.1
beta2  <- mean(data[factor2=='GRUPPO22',1]) - m  # beta.2
# Point-wise estimates of mean duration of travels (model without interaction!)
m11 <- m + tau1 + beta1
m12  <- m + tau1 + beta2
m21 <- m + tau2 + beta1
m22  <- m + tau2 + beta2

## Now we can perform ANOVA WITH INTERACTION (COMPLETE MODEL)
# X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
#     i=1,2 (effect city), j=1,2,3 (effect type)
# Two-ways ANOVA: model with interaction
fit.aov.int <- aov(x ~ factor1 + factor2 + factor1:factor2)
summary.aov(fit.aov.int)

#STIME
# Estimate variances
W <- sum(fit.aov.int$residuals^2)  # SS_res
var <- W/(n-g-b+1)     # SS_res/gdl(res)
var

# We have performed 3 tests:
# 
# 
# 1) Test1:
#   \[ H_0: \tau_1 = \tau_2 = 0 \quad vs \quad H_1: (H_0)^c \]
# namely: 
#   
# H0: The effect factor1 doesn't significantly influence x 
# 
# H1: The effect factor1" significantly influences x
# 
# 
# 2) Test2
# \[ H_0: \beta_1 = \beta_2 = 0 \quad vs \quad H_1: (H_0)^c \]
# namely: 
# 
# H0: The effect factor2 doesn't significantly influence x
# 
# H1: The effect factor2 significantly influences x
#
#
# 3) Test3:
#   \[ H_0: \gamma_{11} = \gamma_{12} = \gamma_{21} = \gamma_{22} = 0 \quad vs \quad H_1: (H_0)^c \] 
# namely: 
#
# H0: There is no significant interaction between the factor1 and factor2 in terms of x
#
# H1: There exists a significant interaction between the factor1 and factor2 in terms of x
# 
# For each test I have to look at the pvalue:
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, the factor/interaction IS significant.
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, the factor/interaction is NOT significant.
#
# If there are some factors/interactions that are NOT significant, 
# we can remove them from the model.



## Now we can perform ANOVA WITHOUT INTERACTION (ADDITIVE MODEL)
# Additive model: WITHOUT interaction
# X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk~N(0,sigma^2), 
#     i=1,2 (effect city), j=1,2,3 (effect type)
fit.aov.ad <- aov(x ~ factor1 + factor2) 
summary.aov(fit.aov.ad)


#STIME
# Estimate variances
W <- sum(fit.aov.int$residuals^2)  # SS_res
var <- W/(n-g-b+1)     # SS_res/gdl(res)
var
# Estimate the great mean mu:
m <- mean(x)
# Estimate tau.i, beta.j:
tau1  <- mean(data[factor1=='GRUPPO11',1]) - m  # tau.1
tau2  <- mean(data[factor1=='GRUPPO12',1]) - m  # tau.2
beta1 <- mean(data[factor2=='GRUPPO21',1]) - m  # beta.1
beta2  <- mean(data[factor2=='GRUPPO22',1]) - m  # beta.2
# Point-wise estimates of mean duration of travels (model without interaction!)
m11 <- m + tau1 + beta1
m12  <- m + tau1 + beta2
m21 <- m + tau2 + beta1
m22  <- m + tau2 + beta2

# We have performed 3 tests:
# 1) Test1:
#   \[ H_0: \tau_1 = \tau_2 = 0 \quad vs \quad H_1: (H_0)^c \]
# namely: 
#   
# H0: The effect factor1 doesn't significantly influence x 
# 
# H1: The effect factor1" significantly influences x
# 
# 
# 2) Test2:
# \[ H_0: \beta_1 = \beta_2 = 0 \quad vs \quad H_1: (H_0)^c \]
# namely: 
# 
# H0: The effect factor2 doesn't significantly influence x
# 
# H1: The effect factor2 significantly influences x
#
# For each test I have to look at the pvalue:
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, the factor/interaction IS significant.
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, the factor/interaction is NOT significant.
#
# If there are some factors/interactions that are NOT significant, 
# we can remove them from the model.


# Note: if only 1 factor is important, you can perform One-Way ANOVA 
# considering only this factor.





#### Two-Ways MANOVA ####

# setting dataset
# labels:
factor1 <- factor(data$......) # ******************************************************************************
factor2 <- factor(data$......) # ******************************************************************************
factor12 <- factor(paste(factor1, factor2, sep='-'))

# note: if factors are dummy variables, you can set labels manually as:
# levels(factor(data$......)) # check the order of the labels
# factor1 <- factor(data$......, labels = c('L','H')) # set labels manually (they must refer to levels above in same order)

# var numerica:
x <- data[, c( , , , , )]       # ******************************************************************************
p <- dim(x)[2]
n <- dim(x)[1]

g <- length(levels(factor1))
b <- length(levels(factor2))
gb <- length(levels(factor12))



### Model WITH interaction (complete model): 
### X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N_p(0,Sigma), [p=2]
###     i=1,2 (effect REL), j=1,2 (effect HPV),
###     X.ijk, mu, tau.i, beta.j, gamma.ij in R^2

### We don't verify the assumptions if we have very few data

# Check Assumptions:

# 1) normality (multivariate) in each interaction of factors (gb test)

Ps12 <- NULL
for (i in 1:gb){
  Ps12 <- c(Ps12, mcshapiro.test(x[factor12 == levels(factor12)[i],])$p)
}
Ps12

# 2) homogeneity of the variance (qualitatively)
S1 <-  cov(x[ factor12==levels(factor12)[1], ])
S2 <-  cov(x[ factor12==levels(factor12)[2], ])
S3 <-  cov(x[ factor12==levels(factor12)[3], ])
# ******************************************************************** they must be gb 

par(mfrow=c(1,g*b))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, 
      breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, 
      breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, 
      breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE, 
      breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))



# Now we can perform Two-Ways MANOVA (COMPLETE model WITH interaction):
fit <- manova( as.matrix(x) ~ factor1 + factor2 + factor1:factor2) 
summary.manova(fit, test="Wilks")

# For each test I have to look at the pvalue:
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, the factor/interaction IS significant.
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, the factor/interaction is NOT significant.

# If there are some factors/interactions that are NOT significant, 
# we can remove them from the model.



# Now we can perform Two-Ways MANOVA (ADDITIVE model WITHOUT interaction):
### Model without interaction (additive model): 
### X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk~N_p(0,Sigma), [p=2]
###     i=1,2 (effect REL), j=1,2 (effect HPV),
###     X.ijk, mu, tau.i, beta.j in R^2

fit2 <- manova( as.matrix(x) ~ factor1 + factor2) 
summary.manova(fit2, test="Wilks")

# For each test I have to look at the pvalue:
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, the factor IS significant.
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, the factor

# If there are some factors that are NOT significant, 
# we can remove them from the model.




## BONFERRONI
# può essere utilizzato per verificare come un fattore influenza un label o un altro 
# fattore influenza l'altro label

alpha <- 0.05 # ************************************************************************

N <- n*g*b 

W <- summary.manova(fit2)$SS$Residuals

# how many comparisons?
k <- g*(g-1)/2*p + b*(b-1)/2*p

qT <- qt(1 - alpha / (2 * k), g*b*n-g-b+1)
# the degrees of freedom of the residuals on the additive model are
# g*b*n-g-b+1

# ************************************************************************ 
n1.1 <- length(factor1==levels(factor1)[1]) # number of observations in level 1 of factor1
n1.2 <- length(factor1==levels(factor1)[2]) # number of observations in level 2 of factor1

mfactor1.1  <- sapply(x[factor1==levels(factor1)[1],],mean)
mfactor1.2  <- sapply(x[factor1==levels(factor1)[2],],mean)
inffactor1 <- mfactor1.2-mfactor1.1 - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n1.1+1/n1.2) )
supfactor1 <- mfactor1.2-mfactor1.1 + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n1.1+1/n1.2) )
# note that if you have more than 2 levels for each factor you should do:
# inffactor1.12 <- mfactor1.2-mfactor1.1 - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n1.1+1/n1.2) )
# inffactor1.13 <- mfactor1.3-mfactor1.1 - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n1.1+1/n1.3) )
# and so on........ # ************************************************************************

# ************************************************************************
n2.1 <- length(factor2==levels(factor2)[1]) # number of observations in level 1 of factor2
n2.2 <- length(factor2==levels(factor2)[2]) # number of observations in level 2 of factor2

mfactor2.1  <- sapply(x[factor2==levels(factor2)[1],],mean)
mfactor2.2  <- sapply(x[factor2==levels(factor2)[2],],mean)
inffactor2 <- mfactor2.2-mfactor2.1 - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n2.1+1/n2.2))
supfactor2 <- mfactor2.2-mfactor2.1 + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/n2.1+1/n2.2))
# note that if you have more than 2 levels for each factor you should do as before... ************************************************************************

IC2 <- list(factor1.1_factor1.2=cbind(inffactor1, supfactor1), 
              factor2.1_factor2.2=cbind(inffactor2, supfactor2))
IC2
# You have to check if 0 is inside/outside the Bonferroni CI. 
# If 0 is inside the Bonferroni CI, the effect of the correspondent factor
# has NO effect on that variable. 


# You can plot the CI's. SEE LABORATORY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

























#### LDA: Univariate Linear Discriminant Analysis ####

n <- dim(data)[1]
p <- dim(data)[2] - 1 # p = 1 in this case!

names(data)[names(data) == 'vecchio nome'] <- 'GRUPPO' #**************************************************
groups <- levels(factor(data$GRUPPO)) 
g <- length(groups)

id1 <- which(data$GRUPPO==groups[1])
id2 <- which(data$GRUPPO==groups[2])
n1 <- length(id1)
n2 <- length(id2)

colori <- rep(0, times = n)
for (i in 1:g){
  colori[data$GRUPPO == groups[i]] <- rainbow(g)[i]
}

# pay attention to which variables you are plotting (data[,1]):
plot(data[,1], rep(0.5, times = n), pch=19, col=colori, ylim = c(0,1), 
     xlab=colnames(data)[1], ylab='')


# LDA assumptions 
#
# 1. if $L=i$, $X_i \sim N(\mu_i, \sigma_i^2)$, $i = A, B$ #******************************************************************************************
# 2. $\sigma_A = \sigma_B$ #******************************************************************************************
# 3. $c(A|B) = c(B|A)$ #******************************************************************************************

# Check Assumptions:
#
# 1. Gaussianity in each group:
#******************************************************************************************
pval1 <- shapiro.test(data[id1,1])$p
pval2 <- shapiro.test(data[id2,1])$p
c(pval1, pval2, ....)
# pvalue VERY SMALL : we cannot reject the null hypothesis, I can NOT assume Gaussianity
# pvalue VERY LARGE : we can reject the null hypothesis, I can assume Gaussianity


# 2. Homoschedasticity between the groups:
var.test(data[id1,1], data[id2,1])$p.value
# pvalue VERY SMALL (<alpha): we cannot reject the null hypothesis, I can NOT assume Homoschedasticity
# pvalue VERY LARGE (>alpha): we can reject the null hypothesis, I can assume Homoschedasticity
# A <- which(data$GRUPPO=='A') #****************************
# B <- which(data$GRUPPO=='B') #****************************
# var.test(data[A,1],data[B,1])
# p-value LARGE: homoschedasticity OK
# p-value SMALL: homoschedasticity NO


# setting priors probabilities and classification costs
# p1: prob of being group1
p1 <- n1/n # only if different by default #*********************************************************************************************************
p2 <- n2/n # only if different by default #*********************************************************************************************************
priors <- c(p1,p2)
# if you have missclassification costs:
# c12: cost you pay if it's 2 but assigned 1
c12 <- ... #*********************************************************************************************************
c21 <- ... #*********************************************************************************************************
priors <- c(p1*c21 / (p1*c21 + p2*c12), p2*c12 / (p1*c21 + p2*c12))


# performing LDA:
priors <- c(, , , ) # only if different by default #*********************************************************************************************************
data.lda <- lda(data$GRUPPO, ~ data[,1] ) #, prior = priors)
data.lda


# plot the classifier:
# you can generalize if more than 2 groups *********************************************************************************************************
P1 <- n1/n
P2 <- n2/n
M1 <- mean(data[id1,1])
M2 <- mean(data[id2,1])
S1 <- var(data[id1,1])
S2 <- var(data[id2,1])
S <- ((n1-1)*S1 + (n2-1)*S2) / (n1+n2-g) # Spooled

x <- seq(range(data[,1]), 0.5)
plot(x, P1*dnorm(x, M1, sqrt(S)) / (P1 * dnorm(x, M1, sqrt(S)) + P2 * dnorm(x, M2, sqrt(S))),
     type = 'l', col = 'blue', ylab = 'estimated posterior')
points(x, P2*dnorm(x, M2, sqrt(S)) / (P1 * dnorm(x, M1, sqrt(S)) + P2 * dnorm(x, M2, sqrt(S))),
     type = 'l', col = 'red')
points(data[id1,1], rep(0, times=length(id1)), pch=16, col='blue')
points(data[id2,1], rep(0, times=length(id2)), pch=16, col='red')
legend(legend=c('P(group1|X=x)', 'P(group2|X=x)'), col = c('blue', 'red'), lty = 1, cex = 0.7)



# classification of a new datum 
new.datum <- c(...,...)  #*********************************************************************************************************
# class associated with the highest posterior probability:
predict(data.lda, new.datum)$class
# posterior probabilities for the classes:
predict(data.lda, new.datum)$posterior
# coordinates of the canonical analysis of Fisher (Fisher’s discriminant scores):
predict(data.lda, new.datum)$x


# classification of a grid of data:
new.data <- data.frame(colnames(data)[1] = c(, , , ,))  #*********************************************************************************************************
# class associated with the highest posterior probability:
predict(data.lda, new.data)$class
# posterior probabilities for the classes:
predict(data.lda, new.data)$posterior
# coordinates of the canonical analysis of Fisher (Fisher’s discriminant scores):
predict(data.lda, new.data)$x



## APER: Actual Predictor Error Rate - with empirical frequencies
# Namely the total number of mistakes over the total number of data.
table(class.true=data$GRUPPO, class.assigned=data.lda$class) # misclassification table
errors <- (data.lda$class != data$GRUPPO)

APER <- sum(errors)/n
APER

## APER: APparent Error Rate - with given priors
priors <- c(, , , ,) #*********************************************************************************************************
misc <- table(class.true=data$GRUPPO, class.assigned=data.lda$class)
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i]
}
APER

# Trivial classifier; classifies always as the most likely class a priori
APER.banale <- # 1-(prior più alto)
APER.banale



## AER: Actual Error Rate - with empirical frequencies
# Compute AER via loo-CV.
# set CV=TRUE for Leave-one-out Cross Validation
data.ldaCV <- lda(data2, data$GRUPPO, CV=TRUE), #prior=priors #*******************************************************************

# misclassification table:
misc <- table(class.true=data$GRUPPO, class.assignedCV=data.ldaCV$class) #*******************************************************************

# SENZA PRIORS
errorsCV <- (data.ldaCV$class != data$GRUPPO)
AERCV <- sum(errorsCV)/n
AERCV

# CON PRIORS
# Errore nel clasificare il gruppo 1 come gruppo 2 
AERCV_lda <- misc[1,2]/(misc[1,1]+misc[1,2])*priors[1]+
  misc[2,1]/(misc[2,1]+misc[2,2])*priors[2]
AERCV_lda


#SE g=3
AER_CV= priors[1]*((misc[1,2]+misc[1,3])/(misc[1,1]+misc[1,2]+misc[1,3]))+
        priors[2]*((misc[2,1]+misc[2,3])/(misc[2,1]+misc[2,2]+misc[2,3]))+
        priors[3]*((misc[3,1]+misc[3,2])/(misc[3,1]+misc[3,2]+misc[3,3]))+
 




#### LDA: Multivariate Linear Discriminant Analysis ####

n <- dim(data)[1]
p <- dim(data)[2] - 1 
names(data)[names(data) == 'vecchio nome'] <- 'GRUPPO' #**************************************************
groups <- levels(factor(data$GRUPPO))
g <- length(groups)

id1 <- which(data$GRUPPO==groups[1])
id2 <- which(data$GRUPPO==groups[2])
id <- c(id1, id2, .....)
n1 <- length(id1)
n2 <- length(id2)

data2 <- data
data2$GRUPPO <- NULL   #*******************************************************************

colori <- rep(0, times = n)
for (i in 1:g){
  colori[data$GRUPPO == groups[i]] <- rainbow(g)[i] #*******************************************************************
}

# plotting data (if bivariate)
plot(data2[,1], data2[,2], pch=19, col=colori, 
     xlab=colnames(data2)[1], ylab=colnames(data2)[2])


# Check Assumptions:
#
# 1. Gaussianity in each group:
#**************************************************************************************
pval1 <- mcshapiro.test(data2[id1,])$p
pval2 <- mcshapiro.test(data2[id2,])$p
# pval3 <- mcshapiro.test(data2[id3,])$p

c(pval1, pval2, ......)

# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, I can NOT assume Gaussianity
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Gaussianity

# 2. Homoschedasticity between the groups:
#**************************************************************************************
S <- cov(data2)
S1 <- cov(data2[id1,])
S2 <- cov(data2[id2,])
#S3 <- cov(data2[id3,])
#... ************************************************************************

# I can only check qualitatively:
par(mfrow=c(1,g))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2, S3), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE,
      breaks = quantile(rbind(S1,S2, S3), (0:100)/100, na.rm=TRUE))
#... image(S3, ...)************************************************************************


# setting priors probabilities and classification costs
# p1: prob of being group1
p1 <- n1/n # only if different by default #*********************************************************************************************************
p2 <- n2/n # only if different by default #*********************************************************************************************************
priors <- c(p1,p2)
# if you have missclassification costs:
# c12: cost you pay if it's 2 but assigned 1
c12 <- ... #*********************************************************************************************************
c21 <- ... #*********************************************************************************************************
priors <- c(p1*c21 / (p1*c21 + p2*c12), p2*c12 / (p1*c21 + p2*c12))


# performing LDA 
priors <- c(, , , ) # only if different by default #*********************************************************************************************************
data.lda <- lda(data2, data$GRUPPO) #, prior = priors) #****************************
data.lda


#Group Priors:
data.lda$prior
#Group Mean:
data.lda$means
#Group Covariances:
lda.pred <- predict(data.lda, data2)
id1_new <- which(lda.pred$class==groups[1])
id2_new <- which(lda.pred$class==groups[2])
cov(data2[id1_new,])
cov(data2[id2_new,])


# graphical representation: plot the partition induced by LDA
plot(data2, xlab=colnames(data2)[1], ylab=colnames(data2)[2], pch=20)
points(data2[id1,], col=rainbow(g)[1], pch=20)
points(data2[id2,], col=rainbow(g)[2], pch=20)
#points(data2[id3,], col=rainbow(g)[3], pch=20) #Se i gruppi sono 3
legend("topright", legend=groups, fill=rainbow(g), cex=.7, bty='n')
points(data.lda$means, pch=4, col=rainbow(g) , lwd=2, cex=1.5)

x <- seq(min(data2[,1]), max(data2[,1]), length=200) 
y <- seq(min(data2[,2]), max(data2[,2]), length=200) 
xy <- expand.grid("metti_nome_colonna_data2[,1]"=x, "metti_nome_colonna_data2[,2]"=y) #*****************
z <- predict(data.lda, xy)$post   # these are P_i*f_i(x,y)

#Se i gruppi sono 2
z1 <- z[,1] - pmax(z[,2]) # P_1*f_1(x,y)-max{P_j*f_j(x,y)} 
z2 <- z[,2] - pmax(z[,1]) # P_2*f_2(x,y)-max{P_j*f_j(x,y)} 

#Se i gruppi sono 3
# z1 <- z[,1] - pmax(z[,2], z[,3]) # P_1*f_1(x,y)-max{P_j*f_j(x,y)} 
# z2 <- z[,2] - pmax(z[,1], z[,3]) # P_2*f_2(x,y)-max{P_j*f_j(x,y)} 
# z3 <- z[,3] - pmax(z[,1], z[,2]) # P_3*f_3(x,y)-max{P_j*f_j(x,y)}

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T) 
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T) 
#contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T) #Se i gruppi sono 3



## APER: Actual Predictor Error Rate - with empirical frequencies
# Namely the total number of mistakes over the total number of data.
lda.pred <- predict(data.lda, data2)
table(class.true=data$GRUPPO, class.assigned=lda.pred$class) # misclassification table
errors <- (lda.pred$class != data$GRUPPO)

APER <- sum(errors)/length(data$GRUPPO) 
APER

## APER: Apparent Error Rate - WITH GIVEN PRIORS
priors <- c(, , , ,) #*********************************************************************************************************
misc <- table(class.true=data$GRUPPO, class.assigned=data.lda$class)  #*******************************************************************
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i]
}
APER


# Trivial classifier; classifies always as the most likely class a priori
APER.banale <- # 1-(prior più alto)
APER.banale



## AER: Actual Error Rate - with empirical frequencies
# Compute AER via loo-CV.
# set CV=TRUE for Leave-one-out Cross Validation
data.ldaCV <- lda(data2, data$GRUPPO, CV=TRUE) #prior=priors #*******************************************************************

# misclassification table:
misc <- table(class.true=data$GRUPPO, class.assignedCV=data.ldaCV$class)

# SENZA PRIORS
errorsCV <- (data.ldaCV$class != data$GRUPPO)
AERCV <- sum(errorsCV)/length(data$GRUPPO) 
AERCV

# CON PRIORS
# Errore nel clasificare il gruppo 1 come gruppo 2 
AERCV <- misc[1,2]/(misc[1,1]+misc[1,2])*priors[1]+
misc[2,1]/(misc[2,1]+misc[2,2])*priors[2]


#SE g=3
AER_CV= priors[1]*((misc[1,2]+misc[1,3])/(misc[1,1]+misc[1,2]+misc[1,3]))+
        priors[2]*((misc[2,1]+misc[2,3])/(misc[2,1]+misc[2,2]+misc[2,3]))+
        priors[3]*((misc[3,1]+misc[3,2])/(misc[3,1]+misc[3,2]+misc[3,3]))+
  
  
  



# classification of a new datum 
new.datum <- c(...,...)  #*********************************************************************************************************
# class associated with the highest posterior probability:
predict(data.lda, new.datum)$class
# posterior probabilities for the classes:
predict(data.lda, new.datum)$posterior
# coordinates of the canonical analysis of Fisher (Fisher’s discriminant scores):
predict(data.lda, new.datum)$x


# classification of a grid of data:
new.data <- data.frame(colnames(data)[1] = c(, , , ,))  #*********************************************************************************************************
# class associated with the highest posterior probability:
predict(data.lda, new.data)$class
# posterior probabilities for the classes:
predict(data.lda, new.data)$posterior
# coordinates of the canonical analysis of Fisher (Fisher’s discriminant scores):
predict(data.lda, new.data)$x






























#### QDA: Univariate Quadratic Discriminant Analysis ####


n <- dim(data)[1]
p <- dim(data)[2] - 1 

groups <- levels(factor(data$GRUPPO)) #******************************************************************************************
g <- length(groups)
#******************************************************************************************
id1 <- which(data$GRUPPO==groups[1]) #*******************************************************************
id2 <- which(data$GRUPPO==groups[2]) #*******************************************************************
id <- cbind(id1, id2, .....) # se non funziona usa c(,)
n1 <- length(id1)
n2 <- length(id2)

data2 <- data
data2$GRUPPO <- NULL #**********************************************


colori <- rep(0, times = n)
for (i in 1:g){
  colori[data$GRUPPO == groups[i]] <- rainbow(g)[i]
}

# plotting data (if bivariate)
plot(data2[,1], data2[,2], pch=19, col=colori, 
     xlab=colnames(data2)[1], ylab=colnames(data2)[2])


# QDA assumptions 
#
# 1. $L=i$, $X_i \sim N(\mu_i, \sigma_i^2)$, $i = A, B$ #******************************************************************************************

# Check Assumptions:
#
# 1. Gaussianity in each group:
#******************************************************************************************
pval1 <- shapiro.test(data[id1,1])$p
pval2 <- shapiro.test(data[id2,1])$p
c(pval1, pval2, ....)
# pvalue VERY SMALL : we can reject the null hypothesis, I can NOT assume Gaussianity
# pvalue VERY LARGE : we cannot reject the null hypothesis, I can assume Gaussianity

# setting priors probabilities and classification costs
# p1: prob of being group1
p1 <- n1/n # only if different by default #******************************************************
p2 <- n2/n # only if different by default #******************************************************
# if you have missclassification costs:
# c12: cost you pay if it's 2 but assigned 1
c12 <- ... #***************************************************************************
c21 <- ... #***************************************************************************
priors <- c(p1*c21 / (p1*c21 + p2*c12), p2*c12 / (p1*c21 + p2*c12))


# performing QDA:
priors <- c(, , , ) # only if different by default #*********************************************************
data.qda <- qda(data2, data$GRUPPO) #, prior = priors) #*******NON SONO SICURA DELLA MODIFICA CHE HO FATTO******
data.qda




# plot the classifier:
# NON SI PUO'
 


## APER: Actual Predictor Error Rate - with empirical frequencies
# Namely the total number of mistakes over the total number of data.
qda.pred <- predict(data.qda, data2) # assigned class to our dataset
table(class.true=data$GRUPPO, class.assigned=qda.pred$class) # misclassification table #*******************************************************************
errors <- (qda.pred$class != data$GRUPPO) #*******************************************************************

APER <- sum(errors)/n
APER

## APER: APparent Error Rate - with given priors
priors <- c(, , , ,) #*********************************************************************************************************
qda.pred <- predict(data.qda, data2) # assigned class to our dataset
misc <- table(class.true=data$GRUPPO, class.assigned=qda.pred$class) #*******************************************************************
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i]
}
APER


# Trivial classifier; classifies always as the most likely class a priori
APER.banale <- # 1-(prior più alto)
APER.banale


## AER: Actual Error Rate - with empirical frequencies
# Compute AER via loo-CV.
# set CV=TRUE for Leave-one-out Cross Validation
data.qdaCV <- qda(data2, data$GRUPPO, CV=TRUE) #prior=priors #*******************************************************************

# misclassification table:
misc <- table(class.true=data$GRUPPO, class.assignedCV=data.qdaCV$class) #*******************************************************************

errorsCV <- (data.qdaCV$class != data$GRUPPO) #*******************************************************************
AERCV <- sum(errorsCV)/n
AERCV

# CON PRIORS
# Errore nel clasificare il gruppo 1 come gruppo 2 
AERCV <- misc[1,2]/(misc[1,1]+misc[1,2])*priors[1]+
misc[2,1]/(misc[2,1]+misc[2,2])*priors[2]


#SE g=3
AER_CV= priors[1]*((misc[1,2]+misc[1,3])/(misc[1,1]+misc[1,2]+misc[1,3]))+
  priors[2]*((misc[2,1]+misc[2,3])/(misc[2,1]+misc[2,2]+misc[2,3]))+
  priors[3]*((misc[3,1]+misc[3,2])/(misc[3,1]+misc[3,2]+misc[3,3]))+
  
  

### PREDICTION of a new datum
new <- c(...,...) #**************
predict(data.qda,new)$class
predict(data.qda,new)$posterior[i] # se si vuole accedere al classifier i-esimo








#### QDA: Multivariate Quadratic Discriminant Analysis ####

n <- dim(data)[1]
p <- dim(data)[2] - 1 

groups <- levels(factor(data$GRUPPO)) #******************************************************************************************
g <- length(groups)
#******************************************************************************************
id1 <- which(data$GRUPPO==groups[1])
id2 <- which(data$GRUPPO==groups[2])
id <- c(id1, id2, .....) #******************************************
n1 <- length(id1)
n2 <- length(id2)

data2 <- data
data2$GRUPPO <- NULL

colori <- rep(0, times = n)
for (i in 1:g){
  colori[data$GRUPPO == groups[i]] <- rainbow(g)[i] #***************************
}

# plotting data (if bivariate)
plot(data2[,1], data2[,2], pch=19, col=colori, 
     xlab=colnames(data2)[1], ylab=colnames(data2)[2])


# Check Assumptions:
#
# 1. Gaussianity in each group:
#**************************************************************************************
pval1 <- mcshapiro.test(data2[id1,])$p
pval2 <- mcshapiro.test(data2[id2,])$p
c(pval1, pval2, ......) #******************************************
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, I can NOT assume Gaussianity
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Gaussianity




# setting PRIORS probabilities and classification costs
# p1: prob of being group1
p1 <- n1/n # only if different by default #*********************************************************************************************************
p2 <- n2/n # only if different by default #*********************************************************************************************************
priors <- c(p1,p2)
# if you have missclassification costs:
# c12: cost you pay if it's 2 but assigned 1
c12 <- ... #*********************************************************************************************************
c21 <- ... #*********************************************************************************************************
priors <- c(p1*c21 / (p1*c21 + p2*c12), p2*c12 / (p1*c21 + p2*c12))

# performing qda:
data.qda <- qda(data2, data$GRUPPO) #, prior = priors) #***************************************************************************************
data.qda
#Group Priors:
data.qda$prior
#Group Mean:
data.qda$means
#Group Covariances:
qda.pred <- predict(data.qda, data2)
id1_new <- which(qda.pred$class==groups[1])
id2_new <- which(qda.pred$class==groups[2])
# id3_new <- which(qda.pred$class==groups[3])

cov(data2[id1_new,])
cov(data2[id2_new,])
# cov(data2[id3_new,])


# plot the classifier
plot(data2, main='', xlab=colnames(data2)[1], ylab=colnames(data2)[2], pch=20)
points(data2[id1,], col=rainbow(g)[1], pch=20)
points(data2[id2,], col=rainbow(g)[2], pch=20)
# points(data2[id3,], col=rainbow(g)[3], pch=20)

legend("topright", legend=groups, fill=rainbow(g))

points(data.qda$means, col=rainbow(g), pch=4, lwd=2, cex=1.5)

x  <- seq(min(data2[,1]), max(data2[,1]), length=200)
y  <- seq(min(data2[,2]), max(data2[,2]), length=200)
xy <- expand.grid("metti_nome_colonna_data2[,1]"=x, "metti_nome_colonna_data2[,2]"=y) #************************

z  <- predict(data.qda, xy)$post  
# Se g = 2
z1 <- z[,1] - z[,2]
z2 <- z[,2] - z[,1]
# SE g > 2
# z1 <- z[,1] - pmax(z[,2], z[,3])    
# z2 <- z[,2] - pmax(z[,1], z[,3])    
# z3 <- z[,3] - pmax(z[,1], z[,2])

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
# contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)



## APER: Actual Predictor Error Rate - with empirical frequencies
# Namely the total number of mistakes over the total number of data.
qda.pred <- predict(data.qda, data2)
table(class.true=data$GRUPPO, class.assigned=qda.pred$class) # misclassification table #***********************************************
errors <- (qda.pred$class != data$GRUPPO) #***********************************************

APER <- sum(errors)/n
APER

## APER: APparent Error Rate - with given priors
priors <- c(, , , ,) #*********************************************************************************************************
qda.pred <- predict(data.qda, data2)
misc <- table(class.true=data$GRUPPO, class.assigned=qda.pred$class) #***********************************************
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i]
}
APER

# Trivial classifier; classifies always as the most likely class a priori
APER.banale <- # 1-(prior più alto)
APER.banale



## AER: Actual Error Rate - with empirical frequencies
# Compute AER via leave one out -CV.
# set CV=TRUE for Leave-one-out Cross Validation
data.qdaCV <- qda(data2, data$GRUPPO, CV=TRUE) #prior=priors  #*******************************************************************

# misclassification table:
misc <- table(class.true=data$GRUPPO, class.assignedCV=data.qdaCV$class) #*******************************************************************

#SENZA PRIORS
errorsCV <- (data.qdaCV$class != data$GRUPPO) #*******************************************************************
AERCV <- sum(errorsCV)/n
AERCV

# CON PRIORS
# Errore nel clasificare il gruppo 1 come gruppo 2 
AERCV <- misc[1,2]/(misc[1,1]+misc[1,2])*priors[1]+
misc[2,1]/(misc[2,1]+misc[2,2])*priors[2]

#SE g=3
AER_CV= priors[1]*((misc[1,2]+misc[1,3])/(misc[1,1]+misc[1,2]+misc[1,3]))+
  priors[2]*((misc[2,1]+misc[2,3])/(misc[2,1]+misc[2,2]+misc[2,3]))+
  priors[3]*((misc[3,1]+misc[3,2])/(misc[3,1]+misc[3,2]+misc[3,3]))+
  
  



## Considerando dei COSTI # CONTROLLARE SE FUNZIONA fino a APER
g1 <-data[data$VARIABILE_CATEGORICA=='...',1:2] #****************************
g2 <-data[data$VARIABILE_CATEGORICA=='...',1:2] #****************************
mcshapiro.test(g1)$pvalue
mcshapiro.test(g2)$pvalue
prior <- c(dim(g2)[1]/(sum(dim(g2)[1],dim(g1)[1])),dim(g1)[1]/(sum(dim(g2)[1],dim(g1)[1])))
p1 <- prior[1]
p2 <- prior[2]
c1 <- #********************* costo g1 
c2 <- #********************* costo g2
# Modified prior to account for the misclassification costs
prior.c <- c(g2=p1*c2/(c1*p2+c2*p1),g1=p2*c1/(c1*p2+c2*p1))
prior.c
# performing qda:
data.qda <- qda(data2, data$GRUPPO, prior = prior.c) 
qda.pred <- predict(data.qda, data2)
misc <- table(class.true=data$GRUPPO, class.assigned=qda.pred$class) #*******************************************************************
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i] * costs[i]
}
APER


## Expected Economic Loss
exp_loss <- (misc[1,2]*c1+misc[2,2]*c2)/sum(misc)





### PREDICTION of a new datum
new.datum <- c(...,...)
# class associated with the highest posterior probability:
predict(data.qda, new.datum)$class
# posterior probabilities for the classes:
predict(data.qda, new.datum)$posterior


z0.new <- data.frame(data$...=..., data$...=...) #************************************
points(z0.new, pch=4, col='springgreen', lwd=2, cex=1.5)

predict(data.qda,z0.new)$class



# Consideri rilevante una delle variabili ai fini della classificazione?
lab <- factor(data$opera)
data$GRUPPO <- NULL
fitted <- manova(as.matrix(data) ~ lab)
summary.aov(fitted)

# Oppure fare un'anova 


#### KNN: Univariate k-nearest neighbor classifier ####

# KNN classifier does NOT requires particular assumptions. 


n <- dim(data)[1]
p <- dim(data)[2] - 1 # in this case p = 1, univariate
k <- #******* #number of neighbours
groups <- levels(factor(data$GRUPPO)) #******************************************************************************************
g <- length(groups)
#******************************************************************************************
id1 <- which(data$GRUPPO==groups[1]) #*******************************************************************
id2 <- which(data$GRUPPO==groups[2]) #*******************************************************************
n1 <- length(id1)
n2 <- length(id2)

data.knn <- knn(train = data[,1], test = new.data, 
                cl = data$GRUPPO,  #*******************************************************************
                k = k,    # number of neighbours 
                prob = T) # if you want to show the proportion of a neighbour in favour of the result 
                          # (empirical relative frequency of each datum to be classified as group1)

data.knn.class <- (data.knn == groups[1]) + 0 # it contains 1 if datum has been classified as groups[1], 0 otherwise.
# probability of being classified as group[1]:
data.knn.class.1 <- ifelse(data.knn.class==1, 
                           attributes(data.knn)$prob, 
                           1-attributes(data.knn)$prob)

# plot the classifier:
plot(x[,1], data.knn.B, type='l', col='black', lty=1)
abline(h = 0.5)
legend(-10, 0.75, legend='knn', lty=c(2,1), col='black')








#### KNN: Multivariate k-nearest neighbor classifier ####

# KNN classifier does NOT requires particular assumptions. 


n <- dim(data)[1]
p <- dim(data)[2] - 1

data2 <- data
data2$GRUPPO <- NULL #*******************************************************************

groups <- levels(factor(data$GRUPPO)) #******************************************************************************************
g <- length(groups)
#******************************************************************************************
id1 <- which(data$GRUPPO==groups[1]) #*******************************************************************
id2 <- which(data$GRUPPO==groups[2]) #*******************************************************************
n1 <- length(id1)
n2 <- length(id2)

k <- #************************************************************************************
data.knn <- knn(train = data2, test = new.data, ## newdata=data2?????
                cl = data$GRUPPO,  #******************************************************************* 
                k = k,    # number of neighbours 
                prob = T) # if you want to show the proportion of a neighbour in favour of the result 
# (empirical relative frequency of each datum to be classified as group1)

data.knn.class <- (data.knn == groups[1]) + 0 # it contains 1 if datum has been classified as groups[1], 0 otherwise.
# probability of being classified as group[1]:
data.knn.class.1 <- ifelse(data.knn.class==1, 
                           attributes(data.knn)$prob, 
                           1-attributes(data.knn)$prob)

# plot the classifier:
plot(data2, xlab=colnames(data2)[1], ylab=colnames(data2)[2], pch=20)
points(data2[id1,], col = rainbow(g)[1], pch = 20)
points(data2[id2,], col = rainbow(g)[2], pch = 20)
points(data2[id3,], col = rainbow(g)[3], pch = 20)
legend("topright", legend=groups, fill=rainbow(g))

x <- seq(min(data2[,1]), max(data2[,1]), length=200) 
y <- seq(min(data2[,2]), max(data2[,2]), length=200) 
xy <- expand.grid("metti nome colonna data2[,1]"=x, "metti nome colonna data2[,2]"=y) #***************************
data.knn.plot <- knn(train = data2, test = xy, cl = data$GRUPPO, k = k) #*******************************************************************
z <- as.numeric(data.knn.plot)
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)



### APER
labels <- factor(data$GRUPPO) #label del groups[1]*****************
# Senza priors
errors <- n - sum(as.numeric(data.knn.class==labels))
APER <- errors/n
APER
# CON PRIORS
predicted <- knn(train = data2, test = data2, cl = labels, k = k)
misclassified <- table(class.true = labels, class.assinged = predicted)
priors <- c(p1,p2,p3)
# ,p3) se ci sono tre gruppi
APER <- 0
for(i in 1:g) 
  APER = APER + sum(misclassified[i,-i])/sum(misclassified[i,]) * priors[i]
APER




#Compute APER with leave one out Cross Validation (stima dell'AER)
#Choose the best k to optimize the misclassification error, via leave-one-out cross-validation
set.seed(321)
labels <- ifelse(data$GRUPPO=='...label del groups[1]', 1, 0) #label del groups[1]*****************


kk <- 10:30
APER <- NULL
for (i in 1:length(kk)){
  k <- kk[i]
  
  right <- 0
  for (j in 1:n){
    id.sel <- j
    train <- data2[-id.sel,]
    test <- data2[id.sel,]
    data.knn <- knn(train = train, test = test, 
                    cl = labels[-id.sel], 
                    k = k,    # number of neighbours 
                    prob = T)
    data.knn.class <- (data.knn == 1) + 0 # it contains 1 if datum has been classified as groups[1], 0 otherwise.
    right <- right + as.numeric(data.knn.class==labels[id.sel])
  }
  APER[i] <- 1- right/n
}

plot(kk, APER, type = 'b', pch=19)
k <- kk[which(APER==min(APER))]

APER[which(APER==min(APER))]


#come clasificare un nuovo punto?
new.point <- data.frame(x=1,y=-4)
data.knn <- knn(train = data2, test = new.point, 
                cl = data$GRUPPO, #*************************
                k = k,    # number of neighbours 
                prob = T)
data.knn
points(new.point,col="orange",pch=19)




#### FisherDA: Univariate Fisher Discriminant Analysis ####

#### FisherDA: Multivariate Fisher Discriminant Analysis ####


n <- dim(data)[1]
p <- dim(data)[2] - 1 

groups <- levels(factor(data$GRUPPO)) #******************************************************************************************
g <- length(groups)
s <- min(g-1, p)
#******************************************************************************************
id1 <- which(data$GRUPPO==groups[1]) #*******************************************************************
id2 <- which(data$GRUPPO==groups[2]) #*******************************************************************
id <- cbind(id1, id2, .....)
n1 <- length(id1)
n2 <- length(id2)

data2 <- data
data2$group <- NULL

colori <- rep(0, times = n)
for (i in 1:g){
  colori[data2$group == groups[i]] <- rainbow(g)[i]
}

# plotting data (if bivariate)
plot(data2[,1], data2[,2], pch=19, col=colori, 
     xlab=colnames(data2)[1], ylab=colnames(data2)[2])


# Check Assumptions:

# 1. Homoschedasticity between the groups:
#**************************************************************************************
bartlett.test(data2[id1,], data2[id2,], data2[id3,],...)$p.value
# pvalue VERY SMALL (<alpha): we can reject the null hypothesis, I can NOT assume Homoschedasticity
# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Homoschedasticity


# useful parameters
# you can generalize if there are more than 2 groups
m <- colMeans(data2)
m1 <- colMeans(data2[id1,])
m2 <- colMeans(data2[id2,])
S1 <- cov(data2[id1,])
S2 <- cov(data2[id2,])
Sp <- ( (n1-1)*S1 + (n2-1)*S2 ) / (n-g) # Spooled
B <- 1/g * (cbind(m1-m) %*% rbind(m1-m) +
              cbind(m2-m) %*% rbind(m2-m)) # Covariance between groups
Spval <- eigen(Sp)$val
Spvec <- eigen(Sp)$vec
Spinv2 <- 1/sqrt(Spval[1]) * Spvec[,1] %*% t(Spvec[,1]) +
  1/sqrt(Spval[2]) * Spvec[,2] %*% t(Spvec[,2])

# note: you have p number of a_i, NOT g
spec.dec <- eigen(Spinv2 %*% B %*% Spinv2)
a1 <- Spinv2 %*% spec.dec$vec[,1] # first canonical decomposition
a2 <- Spinv2 %*% spec.dec$vec[,2] # second canonical decomposition



# Canonical Coordinates of the data:
cc1.data <- as.matrix(data2) %*% a1
cc2.data <- as.matrix(data2) %*% a2
coord.cc <- cbind(cc1.data, cc2.data) 

# Canonical Coordinates of the mean (g objects):
cc.m1 <- c(m1%*%a1, m1%*%a2) # Canonical Coordinates of group1 mean along canonical directions
cc.m2 <- c(m2%*%a1, m2%*%a2) # Canonical Coordinates of group2 mean along canonical directions

# classification of data looking at Euclidean distance
f.class=rep(0, n)
for(i in 1:n) { # for each datum
  # Compute the Euclidean distance of the i-th datum from mean within the groups (g distances)
  dist.m=c(d1 = sqrt(sum((coord.cc[i,]-cc.m1)^2)),
           d2 = sqrt(sum((coord.cc[i,]-cc.m2)^2)))
  # Assign the datum to the group whose mean is the nearest
  f.class[i] = which.min(dist.m)
}
table(class.true=groups, class.assigned=f.class) # misclassification table


## APER: Actual Predictor Error Rate - with empirical frequencies
# Namely the total number of mistakes over the total number of data.
APER <- ( n - sum(diag(table(class.true=groups, class.assigned=f.class))) ) / n


## APER: APparent Error Rate - with given PRIORS
priors <- c(, , , ,) #*********************************************************************************************************
misc <- table(class.true=groups, class.assigned=f.class)
APER <- 0
for ( i in 1:g){
  APER <- APER + sum(misc[i,-i])/sum(misc[i,]) * priors[i]
}
APER



# Classification of a new observation
x.new <- c(,,,,)
# compute the canonical coordinates
cc.new <- c(x.new%%a1, x.new%%a2)
# compute the distance from the means
dist.m <- c(d1=sqrt(sum((cc.new-cc.m1)^2)),
            d2=sqrt(sum((cc.new-cc.m2)^2)))
# assign to the nearest mean
which.min(dist.m)



# Plot the partition induced by the classifier
color.groups <- groups
levels(color.groups) <- c('red','blue')
plot(cc1.data, cc2.data, main='Fisher discriminant analysis',
     xlab='first canonical coordinate', ylab='second canonical coordinate',
     pch=20, col=as.character(color.groups))
legend("topleft", legend=levels(groups), fill=c('red','blue'), cex=.7)
points(cc.m1[1], cc.m1[2], pch=4,col='red' , lwd=2, cex=1.5)
points(cc.m2[1], cc.m2[2], pch=4,col='blue' , lwd=2, cex=1.5)
x.cc <- seq(min(cc1.data),max(cc1.data),len=200)
y.cc <- seq(min(cc2.data),max(cc2.data),len=200)
xy.cc <- expand.grid(cc1=x.cc, cc2=y.cc)
z <- cbind( sqrt(rowSums(scale(xy.cc,cc.m1,scale=FALSE)^2)),
            sqrt(rowSums(scale(xy.cc,cc.m2,scale=FALSE)^2)))
# if g = 2
z1.cc <- z[,1] - z[,2]
z2.cc <- z[,2] - z[,1]
# if g >2
z1.cc <- z[,1] - pmin(z[,2], z[,3])
z2.cc <- z[,2] - pmin(z[,1], z[,3])
z3.cc <- z[,3] - pmin(z[,1], z[,2])

contour(x.cc, y.cc, matrix(z1.cc, 200), levels=0, drawlabels=F, add=T)
contour(x.cc, y.cc, matrix(z2.cc, 200), levels=0, drawlabels=F, add=T)










#### Hierarchical Clustering ####

# if you DO know the labels:
# *********************************************************************************************************
data.lab <- data$...
data$... <- NULL

n <- dim(data)[1]
p <- dim(data)[2]

# graphical representation of data:
plot(data) #solo se p=2
pairs(data)


# setting distance and linkage:
# *********************************************************************************************************
distance <- 'euclidean' # manhattan, canberra
linkages <- c('single', 'average', 'complete', 'ward.D2')

# distance matrix:
data.dist <- dist(data, method=distance)
# plot:
image(1:n,1:n, as.matrix(data.dist), main=paste('metrics: ', distance), asp=1, xlab='', ylab='')


# perform hierarchical clustering:
data.s <- hclust(data.dist, method=linkages[1])
data.a <- hclust(data.dist, method=linkages[2])
data.c <- hclust(data.dist, method=linkages[3])
data.w <- hclust(data.dist, method=linkages[4])

# plot dendograms:
par(mfrow=c(2,2))
plot(data.s, main=paste(distance, ' - ', linkages[1]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(data.a, main=paste(distance, ' - ', linkages[2]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(data.c, main=paste(distance, ' - ', linkages[3]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(data.w, main=paste(distance, ' - ', linkages[4]), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
# note: if you want to set dendogram ylim equal:
#         add: ylim = c(0,VALOREMAX), 
#         take out hang=-0.1, 
#         take out labels=F, 
#         add leaflab='none'

# select the best linkage and best k:
k <- ... # *****************************************************************************************
linkage <- 'single' # average, complete, ward.D2
data.hc <- data.SCEGLINEUNO #s, a, c, w *****************************************************************************************
par(mfrow=c(1,1))
plot(data.hc, main=paste(distance, ' - ', linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.hc, k=k)

# cut dendogram:
clusters <- cutree(data.hc, k=k)


# if you know the true labels:
table(label.true = data.lab, label.cluster = clusters)


#Dimension of the Clusters
table(clusters) 
#Mean within the Clusters


#Centers of the Clusters
center <- matrix(0,k,p)
for (i in 1:k) {
  center[i,] <- colMeans(data[clusters==i, ])
}
colnames(center) <- colnames(data)
center



# graphical representation:
#fai attenzione che il dataset sia formato dataframe, altrimenti non plotta!
colors <- rep(0, times=n)
for (i in 1:k){
  colors[clusters==i] <- rainbow(k)[i]
}
plot(data, col=colors, pch=19, main = paste(distance,'-', linkage))
points(center[1,1], center[1,2], col=rainbow(k)[1], pch=4, lwd=3)  
points(center[2,1], center[2,2], col=rainbow(k)[2] ,pch=4, lwd=3)
#points(center[3,1], center[3,2], col=rainbow(k)[3], pch=4, lwd=3)  
#points(center[4,1], center[4,2], col=rainbow(k)[4] ,pch=4, lwd=3)


# Cophenetic Matrix and Cophenetic Coefficient: (per valutare la classificazione)
coph.hc <- cophenetic(data.hc)
cor.hc <- cor(data.dist, coph.hc)
cor.hc
# The closer it is to 1 and ???1, the better the clustering structure in the
# Cophenetic Matrix is representing a true structure that is in the Distance matrix!
  





# DIFFERENCE BETWEEN THE MEAN
# Perform a statistical test to verify if there is a significant 
# difference between the mean of a variable in the clusters identified.
# UTILIZZO ANOVA! 

data <- read.table('....txt',header=T) #**************************************************
data <- cbind(data,clusters)
#adesso continuo con anova(vedi codici anova), dove metter�:
factor1 <- factor(data$clusters)
x <- data$... #la variabile di cui voglio studiare la media ************************* 





#  k Bonferroni intervals for the mean e/o variances in group i:
# se viene chiesta sia media che varianza k = n° di clusters * 2 * n° variabili di cui si vogliono gli int
# se viene chiesta o media o varianza k = n° di clusters * 1 * n° variabili di cui si vogliono gli int


#SERVE L'ASSUNZIONE DI GAUSSIANITA'
#(Se chiede per la media e per la varianza setto in entrambe k=p, altrimenti k=2*p)
dataor <- data
data <- data[clusters==i,] #************** impostare il cluster che si sta analizzando ****************
n <-  dim(data)[1] #*************
mcshapiro.test(data) # se non èè verificare eliminare l'outlier
data.mean <- sapply(data, mean)
data.cov <- cov(data) 


# Bonferroni CI for the mean at level (1-alpha):
k <- ... # number of intervals I want to compute (set in advance) 
alpha <- #*****************  
cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = data.mean - cfr.t*sqrt(diag(data.cov)/n), #Qui sotto togli diag se sei nell'univariato
            center = data.mean, 
            sup = data.mean + cfr.t*sqrt(diag(data.cov)/n))
Bf



# OPPURE SUPERRRRRRRRRRRR
# CI HIERARCHICAL:
# CI per la media:
alpha <- ... #(1-livello!!!) #*****************************************
k <- ...
level = 1-alpha/k

#Primo Cluster:
t.test(data[clusters == 1, 1], alternative = "two.sided", conf.level = level)$conf.int
t.test(data[clusters == 1, 2], alternative = "two.sided", conf.level = level)$conf.int
t.test(data[clusters == 1, 3], alternative = "two.sided", conf.level = level)$conf.int
t.test(data[clusters == 1, 4], alternative = "two.sided", conf.level = level)$conf.int
#Secondo Cluster:
t.test(data[clusters == 2, 1], alternative = "two.sided", conf.level = level)$conf.int
t.test(data[clusters == 2, 2], alternative = "two.sided", conf.level = level)$conf.int
t.test(data[clusters == 2, 3], alternative = "two.sided", conf.level = level)$conf.int
t.test(data[clusters == 2, 4], alternative = "two.sided", conf.level = level)$conf.int
#Terzo Cluster....
t.test(data[clusters == 3, 1], alternative = "two.sided", conf.level = level)$conf.int
t.test(data[clusters == 3, 2], alternative = "two.sided", conf.level = level)$conf.int
t.test(data[clusters == 3, 3], alternative = "two.sided", conf.level = level)$conf.int
t.test(data[clusters == 3, 4], alternative = "two.sided", conf.level = level)$conf.int



# Bonferroni CI for the VARIANCE at level (1-alpha)
# PRIMO
data_c1<- data[clusters==1,] #************** impostare il cluster che si sta analizzando **************** se non funziona metti which(clusters==1) al posto di (clusters==1) 
n_1 <-  dim(data_c1)[1] #*************
mcshapiro.test(data_c1)
data.mean_1 <- sapply(data_c1, mean) #mean(..) se univariato
data.cov_1 <- cov(data_c1) #var se univariato

n <- n_1
data.cov <- data.cov_1
#Qui sotto togli diag se sei nell'univariato

BFvar_1 <- cbind( inf    =  ((n-1)*diag(data.cov))/qchisq(1-alpha/(2*k), n-1), 
                  center =  diag(data.cov),
                  sup    =  (n-1)*diag(data.cov)/qchisq( alpha/(2*k),n-1))
BFvar_1


# SECONDO
data_c2<- data[clusters==2,] #************** impostare il cluster che si sta analizzando ****************
n_2 <-  dim(data_c2)[1] #*************
mcshapiro.test(data_c2)
data.mean_2 <- sapply(data_c2, mean)
data.cov_2 <- cov(data_c2) 

n <- n_2
data.cov <- data.cov_2
#Qui sotto togli diag se sei nell'univariato

BFvar_2 <- cbind( inf    =  ((n-1)*diag(data.cov))/qchisq(1-alpha/(2*k), n-1), 
                  center =  diag(data.cov),
                  sup    =  (n-1)*diag(data.cov)/qchisq( alpha/(2*k),n-1))

BFvar_2 


# TERZO
data_c3<- data[clusters==3,] #************** impostare il cluster che si sta analizzando ****************
n_3 <-  dim(data_c3)[1] #*************
mcshapiro.test(data_c3)
data.mean_3 <- sapply(data_c3, mean)
data.cov_3 <- cov(data_c3) 

n <- n_3
data.cov <- data.cov_3
#Qui sotto togli diag se sei nell'univariato

BFvar_3 <- cbind( inf    =  ((n-1)*diag(data.cov))/qchisq(1-alpha/(2*k), n-1), 
                  center =  diag(data.cov),
                  sup    =  (n-1)*diag(data.cov)/qchisq( alpha/(2*k),n-1))
BFvar_3





#(Se chiede intervalli for the difference in mean vai a "INDIPENDENT GAUSSIAN POPULATION" )


#### Kmeans Clustering ####

# if you DO know the labels:
# *********************************************************************************************************
data.lab <- data$...
data$... <- NULL

n <- dim(data)[1]
p <- dim(data)[2]

# graphical representation of data:
pairs(data)


# setting k
k <- 2  # *********************************************************************************************************

# performing kmeans clustering
result.k <- kmeans(data, centers = k) # Centers: fixed number of clusters

# see the results:
result.k$cluster        # labels of clusters
result.k$centers        # centers of the clusters
# result.k$totss          # tot. sum of squares
# result.k$withinss       # sum of squares within clusters
# result.k$tot.withinss   # sum(sum of squares in the cluster)
# result.k$betweenss      # sum of squares between clusters
# result.k$size           # dimension of the clusters
# and other useful features (see names(result.k))

# graphical representation
centers <- as.data.frame(result.k$centers)
plot(rbind(data, centers), 
     col = c(result.k$cluster+1, rep('black', times = k)), 
     pch = c(rep(19, times=n), rep(18, times=k)), 
     cex = c(rep(1, times=n), rep(1.75, times = k)))


## HOW to choose k?
#
# 1) method 1: evaluate the variability between the groups with respect to the variability withing the groups
# 2) method 2: evaluate the result of hierarchical clustering

# 1) method 1:
b <- NULL
w <- NULL
for(k in 1:10){
  result.k <- kmeans(Q, k)
  w <- c(w, sum(result.k$wit)) # within sum of squares
  b <- c(b, result.k$bet)      # between sum of squares
}
matplot(1:10, w/(w+b), pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim = c(0,1))
lines(1:10, w/(w+b), type='b', lwd=2)
# look for an elbow etc....

# 2) method 2:
#
# see Hierarchical Clustering



#### Multidimensional Scaling ####

#Dimension of the space in which i want to represent the data
k <- ...  #************************************************************************************** 

# setting distance:
# *********************************************************************************************************
distance <- 'euclidean' # manhattan, canberra
# distance matrix:
data.dist <- dist(data, method=distance)
 

data.map <- cmdscale(data.dist, k=k)

plot(data.map[,1], data.map[,2], type='n', asp=1, axes=FALSE, main="MDS of data",xlab='',ylab='')
text(data.map[,1], data.map[,2], labels=colnames(as.matrix(data.dist)), cex = 0.75, pos = 3)

# compare the original distance matrix d_ij = d(x_i,x_j) and delta_ij = d(y_i,y_j) 
plot(data.dist, dist(data.map))  # Is good if the data are on the bisector


# visualize the most different distances
p <- dim(data.dist)[2]
par(cex = 0.75, mar = c(10,10,2,2))
image(1:p, 1:p, asp=1, abs(as.matrix(dist(data.map)) - as.matrix(data.dist)), axes = F, xlab = '', ylab ='')
axis(1, at = 1:p, labels = colnames(as.matrix(data.dist)), las = 2, cex = 0.75)
axis(2, at = 1:p, labels = colnames(as.matrix(data.dist)), las = 1, cex = 0.75)
box()

# I Compute the "stress": the higher it is, the worse the matching between original distances and their
# geometrical representation through MDS
Stressk <- NULL
for(kk in 1:5)
{
  data.map.k <- cmdscale(data.dist, kk)
  Stress <- (sum( (as.vector(data.dist) - as.vector(dist(data.map.k)))^2)  /
               sum( as.vector(data.map.k)^2))^(1/2)
  Stressk <- c(Stressk, Stress) 
}

plot(1:5,Stressk,xlab='k',ylab='Stress',lwd=2)
# I choose k = .. since i see an elbow.





































#### Multiple Linear Regression ####

pairs(data)
# Control if i have some patterns and i need to transform my data 
#(p.e. if data don't have a clear shape probably i have to y<-log(y), x<-log(x))

n     <- dim(data)[[1]]
y     <- data$...   #*****************************************************************************
reg1  <- data$...   #*****************************************************************************
reg2  <- data$...   #*****************************************************************************

## se nel modello proposto nell'esercizio c'è uno dei regressori al quadrato
## mettere un'altra variabile reg, ad esempio reg2 <-reg1^2 oppure nel modello mettere I(reg^2)


## Model:
# \[ y = \beta_0 + \beta_1 * reg1 + \beta_2 * reg2 + \varepsilon \]

## Model assumptions:
#
# 1) Parameter estimation: $E(\varepsilon) = 0  \quad and \quad Var(\varepsilon) = \sigma^2$
# 2) Inference:            $\varepsilon \sim N(0, \sigma^2)$


fm <- lm(y ~ reg1 + reg2)


## H0: bi=0, H1: bi!=0
summary(fm) 

# Table of coefficients: one line for each beta. In the first column you have 
# the estimate $\hat{\beta}$, then you have the standard error, the value of the 
# t-statistics, p-value of the t-test (remark T-test is the test of significant 
# of the single regressor, $H_0: \beta_i=0 \quad vs \quad H_1: \beta_i \neq 0$).
#
# F-statistic (test of global significance of the regression, namely
# $H_0: \beta_i=0 \quad  \forall \beta_i \quad \quad vs \quad \quad \exists \beta_i \neq 0$)


#NB: if i do not include the intercept (beta0=0) in my model the Rsquared computed in the Summary does not make sense.
#I have to compute another \[ R^2 = 1- \frac{\| {\hat\varepsilon}^2\|}{\|{\hat y}^2\|} \]
#Rsquared <- 1- sum(residuals(fm)^2)/sum(y^2)     #PER TOGLIERE BETa0 faccio lm(y ~ reg1 + reg2 -1 ) #IL -1 toglie l'intercetta


fitted(fm)        # y hat
residuals(fm)     # eps hat   (misfit tra gli yhat calcolati e i veri y)
# 
# coefficients(fm)             # beta_i
# summary(fm)$coefficients[,4] #p-values of beta_i
# vcov(fm)                     # cov(beta_i)
# 
# fm$rank # order of the model [r+1]
# fm$df   # degrees of freedom of the residuals [n-(r+1)]
# 
# hatvalues(fm) # h_ii
# rstandard(fm) # standardized residuals
# # sum(residuals(fm)^2)/fm$df  ###### estimate of sigma^2
#
# The parameters are: 
coefficients(fm) 
sigma <- sd(fm$residuals)

sum(residuals(fm)^2)/fm$df #(estimate of $\sigma^2)$




# Plot the Model:
# (you can plot if you have only one variable, remember to keep all the betas that account for that variable)
plot(reg1,y, las=1, xlim=range(reg1), ylim=range(y))
x <- seq(by=0.1, from=min(reg1), to=max(reg1))
b <- coef(fit)
lines(x, b[1]+b[2]*x)  #+b[3]*x^2)



## Diagnostic of residuals:
par(mfrow=c(2,2))
plot(fm)     

# * Top-Left: scatterplot of residuals against the fitted values 
# ($\hat{y}$ on x-axis, $\hat{\varepsilon}$ on y-axis). 
# The red line corresponds to the general trend, namely the local average of the residuals 
# (to give you an idea if there are specific patterns in your dataset); 
# dashed grey line corresponds to the value 0. 
# Moreover, it points out some possible outliers, enumerated as the number of row 
# corresponding to the original dataset. 
#### We should see homogeneous distribution around 0 with no specific patterns and no particular shapes. 
# 
# * Top-Right: Normal QQ-plot of the quantiles. 
# I have the theoretical quantiles of the Gaussian distribution and the standardized 
# residuals on the y-axis. Again here you see outliers highlighted with numbers. 
#### The scatterplot of the data should be as close as possible to the straight line, 
#### if you see a tail you have the indexes of the extreme observations.
#
# * Bottom-Left: Scale-Location plot. 
# Square root of the standardized residuals against the fitted values. 
#### Here you should see homogeneous distribution with no patterns and no trends.
# 
# * Bottom-Right: Residuals vs Leverage. 
# It provides the standardized residuals against the leverage ($h_{ii}$). 
# You also have the isolines of the Cook's distance represented with red dashed lines. 
#### Using that you can identify the leverage points looking at those points that 
#### lies outside the dashed lines.

shapiro.test(residuals(fm))
# High (> 0.1): there is no statistical evidence to reject H0 (I can assume Gaussianity of data)
# Very low (< 0.05): there is statistical evidence to reject H0 (I can NOT assume Gaussianity of data)
# (this test is a confirmation of the normal QQ-plot: check if there are some heavy tails)



## Inference on the parameters

# Fisher Test is a test of global significance of the regression, namely:
# \[H_0: (\beta_1, \beta_2) = (0,0) \quad vs \quad H_1: (\beta_1, \beta_2) \neq (0,0) \]

# setting the number of covariates you have included in your model 
# (i.e. number of regressors (with intercept))
r <- ... #***********************************************************************************
pippo <- diag(c(0,rep(1,times=r)), r, r)
linearHypothesis(fm, pippo, c(0,0)) 

# This test is the same performed in summary(fm) MMMMMH non so



# Plotting Confidence Region for the regressors:
p <- r  # number of tested coefficient (in this case, p=r)

# center: point estimate
coefficients(fm)[2:r]
# Direction of the axes
eigen(vcov(fm)[2:r,2:r])$vectors


# if r=2, I can plot the ellipse:
xrange <- c(coefficients(fm)[2]- sqrt(p*qf(1-0.05,p,n-(r))), 
            coefficients(fm)[2] +sqrt(p*qf(1-0.05,p,n-(r))))
yrange <- c(coefficients(fm)[3]- sqrt(p*qf(1-0.05,p,n-(r))), 
            coefficients(fm)[3] +sqrt(p*qf(1-0.05,p,n-(r))))
plot(coefficients(fm)[2], coefficients(fm)[3], xlim = xrange, ylim = yrange, asp=1, 
     xlab='beta1', ylab='beta2')
ellipse(coefficients(fm)[2:3], vcov(fm)[2:3,2:3], sqrt(p*qf(1-0.05,p,n-(r))))
abline(v=0)
abline(h=0)
# note that if the Ellipse is stretched, regressors might be collinear.





# the model explain the dependance between th e variable and the regressor?
points(reg,fitted(fm),col='blue', pch=19) # ********* reg inserire quale
summary(fm)$r.squared







## Bonferroni intervals (of level 1-alpha) #"Intervallo in cui stanno i regressori"
alpha <- 0.05 #***************************************************************************************************************************
confint(fm, level= 1-alpha/p)[2:r,]  # r: number of regressors (minus beta0)
# Note: confint() returns the confidence intervals one-at-a-time;
# to build BonfCI with global level 1-alpha we need to include Bonf-correction (level= 1-alpha/p)


### Test:
# H0: (a1*beta0+b1*beta1+c1beta2, a2beta0+b2beta1+c2beta2, ...) == (d1,d2, ...) vs H1: (a1*beta0+b1*beta1+c1beta2, a2beta0+b2beta1+c2beta2, ...) != (d1,d2, ...)   
# C*[beta0,beta1, beta2,...]= c(d1,d2,...): test linear combination of the coefficients
 C <- rbind(c(0,1,1), # beta1 + beta2   #************************************************
            c(1,0,0), # beta0
            c(0,1,0), # beta1
            c(0,0,1)) # beta2
d <- c(0,0,..)                         #************************************************

linearHypothesis(fm, C, d)











# INCREMENTO dipedente dal REGRESSORE
# Waste = A + B * t  + C * (1-cos(2pi / 12 * t)) + eps
# The University of Cantabria considered that the GROWTH attributable to
# residents is quantifiable in an increase of 10 tons per month (REGRESSORE).
fit <- lm(rifiuti ~ mese + I(1 - cos(2*pi/12*mese)))

linearHypothesis(fit,rbind(c(0,1,0)),10)

# or (from the summary)
summary(fit)
t <- (coef(fit)[2]-10)/sqrt(diag(vcov(fit))[2])
t
pval <- 2*(1-pt(t,29-(2+1)))
pval

### question e)
rifiuti.vinc <- rifiuti - 10*mese

fit2 <- lm(rifiuti.vinc ~ I(1 - cos(2*pi/12*mese)))
summary(fit2)






# AGGIORNARE
### Se viene chiesto se il reg_i è m volte il reg_j

C <- rbind(c(0,0,1,-m))  # 1 ed m nelle posizioni dei regressori i e j ***********************************************
# beta2
d <- c(0)                #************************************************

linearHypothesis(fm, C, d)

# Aggiornare i coefficienti
fm$coefficients[i]=m*fm$coefficients[j]

fm <- lm(y ~ reg1 +reg3+ reg4)
summary(fm)





# Homework
# Build the associated confidence region





## CONFIDENCE INTERVALS for the mean & prediction of a new obs
#In this case i need the assumpion of Gaussianity.
new.datum <- data.frame(reg1=..., reg2=...) #********************************************
alpha <- 0.05 #*****************************************************************************
# Conf. int. for the mean
Conf <- predict(fm, new.datum, interval='confidence', level=1-alpha)  
Conf
# Pred. int. for a new obs
Pred <- predict(fm, new.datum, interval='prediction', level=1-alpha)  
Pred
# Pointwise prediction
pointwise_prediction <- (Pred[2]+Pred[3])/2 # cioè (lwr+upw)/2




plot(data, xlab='x', ylab='y', las=1, xlim=range(reg1), ylim=range(y))
x <- seq(range(reg1),by=0.1)
b <- coef(fm)
lines(x, b[1]+b[2]*x) #+b[3]*x^2)
points(new.datum$reg1,Conf[1], pch=19)
segments(new.datum$reg1,Pred[2], new.datum$reg1,Pred[3], col='gold', lwd=2)
segments(new.datum$reg1,Conf[2], new.datum$reg1,Conf[3], col='red', lwd=2)
points(new.datum$reg1,Conf[2], pch='-', col='red', lwd=2)
points(new.datum$reg1,Conf[3], pch='-', col='red', lwd=2)
points(new.datum$reg1,Pred[2], pch='-', col='gold', lwd=2)
points(new.datum$reg1,Pred[3], pch='-', col='gold', lwd=2)





# We can repeat these for values of speed between 0 and 30
#In this case i need the assumpion of Gaussianity.
# (prediction and confidence one-at-the-time intervals)
# (remember: they are point-wise intervals! NOT bands!!)
# new.data <- data.frame(cbind(reg1=seq(range(reg1), length=100), 
#                          reg2=seq(range(reg2), length=100)))  #if i want a grid
new.data <- data.frame(cbind(reg1=...,  reg2=...))              #for only 1 data
Conf <- predict(fm, new.data, interval='confidence')
Pred <- predict(fm, new.data, interval='prediction')

plot(cars, xlab='x', ylab='y', las=1, xlim=range(reg1), ylim=range(y))
lines(new.data[,1], Conf[,'fit'])
lines(new.data[,1], Conf[,'lwr'], lty=2, col='red', lwd=2)
lines(new.data[,1], Conf[,'upr'], lty=2, col='red', lwd=2)

lines(new.data[,1], Pred[,'lwr'], lty=3, col='gold', lwd=2)
lines(new.data[,1], Pred[,'upr'], lty=3, col='gold', lwd=2)












# ESEMPIO
# Model B: mu(t) = alpha1 + beta1 * t for 1768 <= t <= 1815
#          mu(t) = alpha2 + beta2 * t for t <= 1767 or t> = 1816;
# DIVENTA
# D <- ifelse(Anno>=1768 & Anno<=1815, 1, 0)
# fitB <- lm(Numero ~ D + Anno + D*Anno )
# summary(fitB)

# alpha <- c(alpha1=coef(fitB)[1]+coef(fitB)[2],alpha2=coef(fitB)[1])
# alpha
# beta  <- c( beta1=coef(fitB)[3]+coef(fitB)[4], beta2=coef(fitB)[3])
# beta

# (c)
#    using the model (b) and Bonferroni's inequality, they provide 2 90% 
#    global confidence intervals for the mean and the variance of the number
#    of works included in the index in the year 1800;
# 
# k <- 2
# alpha <- .1
# n <- dim(index)[1]
# r <- 3
# 
# Z0   <- data.frame(D=1, Anno=1800)
# ICBmean <- predict(fitB, Z0, interval='confidence',level=1-alpha/k) 
# ICBmean
# 
# e <- residuals(fitB)
# ICBvar <- data.frame(L=t(e)%*%e/qchisq(1-alpha/(2*k),n-(r+1)),
#                      U=t(e)%*%e/qchisq(alpha/(2*k),n-(r+1)))
# ICBvar

# (d)
# using the model (b), provide a 90% confidence interval for the difference 
# between the average number of works added to the Index in 1816 and average 
# number of works added in 1815.
# a <- c(0,-1,1,-1815)
# Bf <- c('1816-1815_L'= t(a) %*% coefficients(fitB) - sqrt(t(a) %*% vcov(fitB) %*% a) * qt(1 - alpha/2, n-(r+1)),
#         '1816-1815_U'= t(a) %*% coefficients(fitB) + sqrt(t(a) %*% vcov(fitB) %*% a) * qt(1 - alpha/2, n-(r+1)) )
# Bf
















#### Linear Regression: ANCOVA ####


n     <- dim(data)[[1]]
y     <- data$...   #*****************************************************************************
reg1  <- data$...   #*****************************************************************************
reg2  <- data$...   #*****************************************************************************



## se nel modello proposto nell'esercizio c'è uno dei regressori al quadrato
## mettere un'altra variabile reg, ad esempio reg2 <-reg1^2 oppure nel modello mettere I(reg^2)


#Dummy variables (for g group I need g-1 dummy variables)

#Caso A) Se ogni colonna ? un gruppo Reg<-c(data[,1], data[,2], ...) e poi dummy1<-rep(c(1,0), c(n1,n-n1)), dummy2<-rep(c(0,1,0), c(n1,n2,n3))
#     x <- rep(inizio:fine,g) e infine data <- data.frame(Reg=Reg, x=x, d1=dummy1, d2=dummy2))
#Caso B) Se il gruppo viene specificato da una variabile uso d1 <- ifelse(data$GRUPPO=="nomei", 1, 0)
#Caso C) A volte c'e' gia' e non faccio nulla

### SE CI SONO TRE GRUPPI FARE due DUMMY ???





# Model:
# (Se ho 2 regressori oltre a Beta0 e i gruppi sono 2)
#  \[ Reg = b_0+b_1*reg1+b_2*reg2 +b_3*d1 +b_4*d1*reg1+b_5*d1*reg2 + \varepsilon, \quad \varepsilon \sim N(0, \sigma^2)\]
# Indeed:
#   \begin{align*}
# & \beta_0^{group1}=b_0;       &  \beta_1^{group1}&=b_1;   & \beta_2^{group1}&=b_2;\\
# & \beta_0^{group2}=b_0+b_3;   &  \beta_1^{group2}&=b_1+b_4 & \beta_2^{group2}&=b_2+b_5;;\\
# \end{align*}
#
#
#(Se ho 1 regressore oltre a Beta0 e i gruppi sono 3)
#I can perform the estimation of the linear model. 
#  \[ Reg = b_0+b_1*d1+b_2*d2 +b_3*reg1+b_4*d1*reg1+b_5*d2*reg1 + \varepsilon, \quad \varepsilon \sim N(0, \sigma^2)\]  
# Indeed:
#   \begin{align*}
# & \beta_0^{group1}=b_0;       &  \beta_1^{group1}&=b_3; \\
# & \beta_0^{group2}=b_0+b_1;   &  \beta_1^{group2}&=b_3+b_4;\\
# & \beta_0^{group3}=b_0+b_2;  &  \beta_1^{group3}&=b_3+b_5
# \end{align*}
#
#
# Model:
#(Se ho 1 regressore oltre a Beta0 e i gruppi sono 2)
#  \[ Reg = b_0+b_1*reg1+b_2*d1 +b_3*d1*reg1 \varepsilon, \quad \varepsilon \sim N(0, \sigma^2)\]
# Indeed:
# \begin{align*}
# & \beta_0^{group1}=b_0;       &  \beta_1^{group1}&=b_1;     \\ 
# & \beta_0^{group2}=b_0+b_2;   &  \beta_1^{group2}&=b_1+b_3   \\
# \end{align*}
#
#


#(se g1=2, r=2 aggiungo reg2 ed elimino d2) )
fit <- lm(y ~ reg1 + reg2 + d1 + reg1:d1 + reg2:d1)



#(Se g1=3, r=1)   
#fit <- lm(y ~ d1 + d2 + reg1 + reg1:d1 + reg1:d2)

summary(fit)

# fitted(fit)        # y hat
# residuals(fit)     # eps hat   (misfit tra gli yhat calcolati e i veri y)
# 
# coefficients(fit)  # beta_i
# summary(fit)$coefficients[,4] #p-values of beta_i
# vcov(fit)          # cov(beta_i)
# 
# fit$rank # order of the model [r+1]
# fit$df   # degrees of freedom of the residuals [n-(r+1)]
# 
# hatvalues(fit) # h_ii
# rstandard(fit) # standardized residuals
# # sum(residuals(fit)^2)/fit$df  ###### estimate of sigma^2
#
# The parameters are: 
coefficients(fit) 
sum(residuals(fit)^2)/fit$df # \sigma^2
# 
# PER TROVARE I PARAMETRI SOMMA I RISPETTIVI VALORI CHE ESCONO DAL COEFF(fit) (da cui escono le b)




# Plot the regression line (2 dummy + N regressors ) (only in 2 dimensions)
col=rep(NA, n)
col[which(d1==0 & d2==0)]=rainbow(g1*g2)[1] #If i have 2 dummy (2 cat.l variables with 2 groups each or 1 cat. variable with 3 groups)                            #         i have to provide all the interactions ()
col[which(d1==1 & d2==0)]=rainbow(g1*g2)[2] #I HAVE TO PROVIDE ALL THE INTERACTIONS! (g1*g2)
#col[which(d1==0)& d2==1 )]=rainbow(g1*g2)[3]
#col[which(d1==1)& d2==1 )]=rainbow(g1*g2)[4]


plot(reg1, y, main='Scatterplot of Y vs Reg', lwd=2, 
     xlab='Reg', ylab='Y', col = col)

coef <- fit$coef
#abline(coef beta0 + dummy, coef reg1 + reg1:dummy)  #DEVO FARE g1*g2 plot
abline(coef[1],coef[2],lwd=2,col=...) #*************************************************************************************
abline(coef[1]+coef[3],coef[2],lwd=2,col=...) #*****************************************************************************
abline(coef[1]+coef[4],coef[2],lwd=2,col=...) #*****************************************************************************
abline(coef[1]+coef[3]+coef[4],coef[2],lwd=2,col=...) #*********************************************************************



# PLOT THE REGRESSION LINE (1 dummy + 1 reg) (vedi Ex4 120919)

plot(data$..., data$...,xlab="...",ylab="observed data",type="l") #dati originali che voglio plottare***************

x <- data[which(data$GRUPPO == "...."),] # GRUPPO a cui corrisponde dummy 1******************************************
newdata <- data.frame(cbind(reg1 = x[,2] ,d1 = 1)) # sostituisco a 2 la colonna a cui corrisponde reg1 ***************************
y_t <- predict(fit, newdata)
points(x$..., y_t, xlab="...",ylab="observed data",type="l", col = "blue") #colonna di reg1 ****************************

x <- data[which(data$GRUPPO == "..."),]  #GRUPPO a cui corrisponde dummy 0******************************************
newdata2 <- data.frame(cbind(reg1 = x[,2] ,d1 = 0)) # sostituisco a 2 la colonna a cui corrisponde reg1 ***************************
y_t2 <- predict(fit, newdata2) 
points(x$..., y_t2, xlab="...",ylab="observed data",type="l", col = "red") #colonna di reg1 ****************************



## Diagnostic of residuals:
par(mfrow=c(2,2))
plot(fit)     

# * Top-Left: scatterplot of residuals against the fitted values 
# ($\hat{y}$ on x-axis, $\hat{\varepsilon}$ on y-axis). 
# The red line corresponds to the general trend, namely the local average of the residuals 
# (to give you an idea if there are specific patterns in your dataset); 
# dashed grey line corresponds to the value 0. 
# Moreover, it points out some possible outliers, enumerated as the number of row 
# corresponding to the original dataset. 
#### We should see homogeneous distribution around 0 with no specific patterns and no particular shapes. 
# 
# * Top-Right: Normal QQ-plot of the quantiles. 
# I have the theoretical quantiles of the Gaussian distribution and the standardized 
# residuals on the y-axis. Again here you see outliers highlighted with numbers. 
#### The scatterplot of the data should be as close as possible to the straight line, 
#### if you see a tail you have the indexes of the extreme observations.
#
# * Bottom-Left: Scale-Location plot. 
# Square root of the standardized residuals against the fitted values. 
#### Here you should see homogeneous distribution with no patterns and no trends.
# 
# * Bottom-Right: Residuals vs Leverage. 
# It provides the standardized residuals against the leverage ($h_{ii}$). 
# You also have the isolines of the Cook's distance represented with red dashed lines. 
#### Using that you can identify the leverage points looking at those points that 
#### lies outside the dashed lines.



shapiro.test(residuals(fit))
# High (> 0.1): there is no statistical evidence to reject H0 (I can assume Gaussianity of data)
# Very low (< 0.05): there is statistical evidence to reject H0 (I can NOT assume Gaussianity of data)
# (this test is a confirmation of the normal QQ-plot: check if there are some heavy tails)





### Test on regressors:
# H0: (a1*beta0+b1*beta1+c1beta2, a2beta0+b2beta1+c2beta2, ...) == (d1,d2, ...) 
# vs H1: (a1*beta0+b1*beta1+c1beta2, a2beta0+b2beta1+c2beta2, ...) != (d1,d2, ...)   
# C*[beta0,beta1, beta2,...]= c(d1,d2,...): test linear combination of the coefficients
C <- rbind(c(0,1,1), # beta1 + beta2   #************************************************
           c(1,0,0), # beta0
           c(0,1,0), # beta1
           c(0,0,1)) # beta2
d <- c(0,0,..) 
linearHypothesis(fit, C, d)
#Test per vedere se il gruppo influisce: tutto 0 tranne i bi moltiplicati per dummy (anche l'interazione) ************
#Test per vedere se il regressore e' influente: tutto 0 tranne i bi moltiplicati per il regressore
#Test per vedere se il gruppo influisce sul regressore i: tutto 0 tranne i bi moltiplicati per il regressore i E le dummy







# Se viene chiesto se il MASSIMO della variable y è per un valore specifico del reg1
# plot(reg1,y, las=1, xlim=range(reg1), ylim=range(y))
# x <- seq(by=0.1, from=min(reg1), to=max(reg1))
# b <- coef(fit)
# lines(x, b[1]+b[2]*x+b[3]*x^2) # il secondo elemento è l'espressione della funzione
## l'andamento è a parabola quindi posso controllare se la derivata nel valore specifico di reg1
# è nulla

# C <- rbind(.. coefficienti della derivata della funzione ... )  esempio C <- rbind(0,1,2*valore di reg 1(era 61))
# d <- c(0)
# linearHypothesis(fit, C, d)
## Se si bisogna aggiornare il parametro:
# fit <- lm(y ~ I(-2*61*reg1 + reg2))









# MASSIMO uguale a un valore dipendentemente dal GRUPPO
g1 <- lm(waiting.time ~ day.of.the.week + I(1 + cos(4*pi*day/365)))

incremento <- 60
linearHypothesis(g1, c(1,1,2), incremento) # inserire  i coefficienti per avere il massimo della funzione 
#So we propose the reduced model
# y = alpha_0 + beta(1 + cos(4 * pi * day/365)) + I(g = 1)*(60 - 2*beta) + eps
W <- ifelse(day.of.the.week=="weekend",1,0)
wait <- waiting.time
wait[which(W==1)] <- wait[which(W==1)] - 60
g_red <- lm(wait ~ I(1-W) + I((1 + cos(4 * pi * day/365)) - 2*W) - 1)
summary(g_red)
shapiro.test(g_red$residuals)
params_red <- c(alpha_0 = g_red$coefficients[1],
                alpha_1 = 60 - 2*g_red$coefficients[2],
                beta = g_red$coefficients[2])
s_red <- summary(g_red)$sigma
z_new <- data.frame(W = 0, day = 238)
predict(g_red, z_new, interval = "prediction", level = 0.95)+60*(z_new$W==1) # predizione di un nuovo dato



# INCREMENTO DIPENDENTE DAL RGRUPPO
# AGGIORNARE





# there is a significant difference between the groups of the dummy, 
# in the increase of the mean sales along time. 
C <- rbind(c()) # mettere il vettore di 0 e un 1 dove c'è reg:dummy
d <- c(0,0,..) 
linearHypothesis(fit, C, d)









## Confidence intervals for the mean & prediction of a new obs
#In this case i need the assumpion of Gaussianity.
new.datum <- data.frame(reg1=..., reg2=..., d1= ..., d2= ...)  #mettere f(reg1) se reg1 NON ? LINEARE! #***************************
alpha <- 0.05 #*****************************************************************************
# Conf. int. for the mean
Conf <- predict(fit, new.datum, interval='confidence', level=1-alpha)  
Conf
# Pred. int. for a new obs
Pred <- predict(fit, new.datum, interval='prediction', level=1-alpha)  
Pred
# Pointwise prediction
pointwise_prediction <- (Pred[2]+Pred[3])/2 # cioè (lwr+upw)/2
pointwise_prediction
# Condifence interval for the maximum
# # Il massimo ? ottenuto ad esempio per t = 365 e wet = 0, sea = 1
# shapiro.test(residuals(fitted))
# new = data.frame(t=365,sea=1,wet=0)
# interval = predict(fitted,new,interval="confidence",level=0.99)






# PLOT
plot(data, xlab='x', ylab='y', las=1, xlim=range(reg1), ylim=range(y))
x <- seq(range(reg1),by=0.1)
b <- coef(fm)
lines(x, b[1]+b[2]*x*d1...) #+b[3]*x^2)  #*************************************************
points(new.datum$reg1,Conf[1], pch=19)
segments(new.datum$reg1,Pred[2], new.datum$reg1,Pred[3], col='gold', lwd=2)
segments(new.datum$reg1,Conf[2], new.datum$reg1,Conf[3], col='red', lwd=2)
points(new.datum$reg1,Conf[2], pch='-', col='red', lwd=2)
points(new.datum$reg1,Conf[3], pch='-', col='red', lwd=2)
points(new.datum$reg1,Pred[2], pch='-', col='gold', lwd=2)
points(new.datum$reg1,Pred[3], pch='-', col='gold', lwd=2)





# We can repeat these for values of speed between 0 and 30
# (prediction and confidence one-at-the-time intervals)
# (remember: they are point-wise intervals! NOT bands!!)
#In this case i need the assumpion of Gaussianity.
new.data <- data.frame(cbind(reg1=seq(range(reg1), length=100), 
                             reg2=seq(range(reg2), length=100),
                             d1=c(...), d2=c(...)))
Conf <- predict(fm, new.data, interval='confidence')
Pred <- predict(fm, new.data, interval='prediction')

plot(cars, xlab='x', ylab='y', las=1, xlim=range(reg1), ylim=range(y))
lines(new.data[,1], Conf[,'fit'])
lines(new.data[,1], Conf[,'lwr'], lty=2, col='red', lwd=2)
lines(new.data[,1], Conf[,'upr'], lty=2, col='red', lwd=2)

lines(new.data[,1], Pred[,'lwr'], lty=3, col='gold', lwd=2)
lines(new.data[,1], Pred[,'upr'], lty=3, col='gold', lwd=2)









#### Linear Regression: PCA solution ####

n     <- dim(data)[[1]]
y     <- data$...   #*****************************************************************************
reg1  <- data$...   #*****************************************************************************
reg2  <- data$...   #*****************************************************************************
r <- ... # number of regressors (beta0 included) #************************************************

fm <- lm(y ~ reg1 + reg2) #***********************************************************************

# check collinearity: compute Variance Inflation Factor
vif(fm)
# if reg_i has vif_i>10, it's consider high (collinear)


data.reg <- cbind(reg1,reg2, ...) #****************************************************************
# performing PCA to solve the problem of collinearity:
pc.data <- princomp(data.reg, scores=TRUE) 
summary(pc.data)
pc.data$loadings


# plotting first nr = 2 loadings
nr <- 2    # number of components you want to visualize
par(mfrow = c(nr,1))
for(i in 1:nr) barplot(pc.data$loadings[,i], ylim = c(-1, 1), main = paste('PC', i))


# plotting results (Screeplot on the right)
varmax <- max(var(data.reg[,1:dim(data.reg)[2]]))
varmax_pc <- max(pc.data$sd)
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.data, las=2, main='Principal components', ylim=c(0,varmax_pc^2))
barplot(sapply(data.reg,sd)^2, las=2, main='Original Variables', ylim=c(0,varmax),
        ylab='Variances')
plot(cumsum(pc.data$sd^2)/sum(pc.data$sd^2), type='b', axes=F, 
     xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data.reg),labels=1:ncol(data.reg),las=2)

# choose k: number of components you want to keep (elbow, threshold...)
k <- 2 #**************************************************************************************************


# Now we estimate the model by inserting the PCs instead of the original regressors: Model:
# \[ y = b_0 + b_1*PC_1 + b_2*PC_2 + ... + b_k*PC_k + \varepsilon, \quad \varepsilon \sim N(0, \sigma^2)\]
reg1.pc <- pc.data$scores[,1] # projection 
reg2.pc <- pc.data$scores[,2] # projection 
# and so on for all k reg... #*****************************************************************************

# Model:
fm.pc <- lm(y ~ reg1.pc + reg2.pc + ....) #****************************************************************
summary(fm.pc)
# NB: look at the p-values and eventually remove them one by one.


# plotting the regression line in space PC1 vs y:
plot(reg1.pc,y, xlab='PC1', ylab='y', las=1, xlim=range(reg1.pc), ylim = range(y)) 
x1 <- seq(range(reg1.pc), by=1)
b <- coef(fm.pc)
lines(x, b[1]+b[2]*x) 


# I can go back to the original variables by using the expressions of the PCs. 
# So I can transform the coefficients I have obtained with PCA to the coefficients in the original system.
m1 <- mean(reg1)
m2 <- mean(reg2)
# and so on for all reg...  #*****************************************************************************
m <- c(m1,m2,...) #***************************************************************************************

beta0 <- coefficients(fm.pc)[1]
for ( i in 2:k+1){
  for (j in 2:r){
    beta0 <- beta0 - coefficients(fm.pc)[i]*pc.data$load[j-1,i-1]*m[j-1]
  }
}

beta1 <- 0
for (i in 2:k+1){
  beta1 <- beta1 + coefficients(fm.pc)[i]*pc.data$load[1,i-1]
}
  
beta2 <- 0
for (i in 2:k+1){
  beta2 <- beta2 + coefficients(fm.pc)[i]*pc.data$load[2,i-1]
}

#********************************************************************************************
# and so on for all betas
  
c(beta0 = as.numeric(beta0),
  beta1 = as.numeric(beta1),
  beta2 = as.numeric(beta2), 
  beta3 = ...) #*****************************************************************************


# plotting our model in the original space (only in dimension 2): reg1 vs y
x <- seq(range(reg1), len=100)
plot(reg1, y, xlab='reg1', ylab='y', las=1) 
lines(x, beta0+beta1*reg1) # add the regressors that are function of reg1



## Diagnostic of residuals 
par(mfrow=c(2,2)) 
plot(fm.pc)
# * Top-Left: scatterplot of residuals against the fitted values 
# ($\hat{y}$ on x-axis, $\hat{\varepsilon}$ on y-axis). 
# The red line corresponds to the general trend, namely the local average of the residuals 
# (to give you an idea if there are specific patterns in your dataset); 
# dashed grey line corresponds to the value 0. 
# Moreover, it points out some possible outliers, enumerated as the number of row 
# corresponding to the original dataset. 
#### We should see homogeneous distribution around 0 with no specific patterns and no particular shapes. 
# 
# * Top-Right: Normal QQ-plot of the quantiles. 
# I have the theoretical quantiles of the Gaussian distribution and the standardized 
# residuals on the y-axis. Again here you see outliers highlighted with numbers. 
#### The scatterplot of the data should be as close as possible to the straight line, 
#### if you see a tail you have the indexes of the extreme observations.
#
# * Bottom-Left: Scale-Location plot. 
# Square root of the standardized residuals against the fitted values. 
#### Here you should see homogeneous distribution with no patterns and no trends.
# 
# * Bottom-Right: Residuals vs Leverage. 
# It provides the standardized residuals against the leverage ($h_{ii}$). 
# You also have the isolines of the Cook's distance represented with red dashed lines. 
#### Using that you can identify the leverage points looking at those points that 
#### lies outside the dashed lines.

shapiro.test(residuals(fm.pc))
# High (> 0.1): there is no statistical evidence to reject H0 (I can assume Gaussianity of data)
# Very low (< 0.05): there is statistical evidence to reject H0 (I can NOT assume Gaussianity of data)
# (this test is a confirmation of the normal QQ-plot: check if there are some heavy tails)
























#### Linear Regression: ELASTIC NET solution ####

# \[ \frac{1-\alpha}{2} \|\beta \|_2^2 + \alpha \|\beta \|_1, \quad \alpha \in [0,1] \]
#
# - if $\alpha=0$: Ridge regression
# - if $\alpha=1$: Lasso regression



n     <- dim(data)[[1]]
y     <- data$...   #*****************************************************************************
reg1  <- data$...   #*****************************************************************************
reg2  <- data$...   #*****************************************************************************
r <- ... # number of regressors (beta0 included) #************************************************

fm <- lm(y ~ reg1 + reg2) #***********************************************************************

# check collinearity: compute Variance Inflation Factor
vif(fm)
# if reg_i has vif_i>10, it's consider high (collinear)


# build matrix of predictors:
x <- model.matrix(y~reg1+reg2+...)[,-1] #*****************************************************************************


## if you know lambda:
lambda <- ... # bestlam.net #*****************************************************************************

# fitting net model 
fit.net <- glmnet(x, y, lambda = lambda, alpha = 0) # if alpha = 0: net

coef.net <- coef(fit.net) # beta coefficients
# yhat.lm <- cbind(rep(1,n), reg1, reg2)%*%coef(fm)      # LM fitted values 
yhat.en  <- cbind(rep(1,n), reg1, reg2)%*%coef.net    # net fitted values


# graphical representation: lm.net()    (and comparison with lm())
plot(reg1, yhat.en, type='l', lty=1, col=grey.colors(length(lambda)), lwd=2) 
points(reg1, y, pch=1, cex=.8)
# matlines(reg1, yhat.lm, type='l', lty=4, lwd=2, ylab='y',xlab='reg1') # comparison with lm
# legend("topleft",c("lm","net"),lty=c(4,1),col=c("black",grey.colors(length(lambda))), lwd = 2)


## if you do NOT know lambda:
lambda.c <- seq(0,10,0.01) #************************************************************************
fit.net <- glmnet(x, y, lambda = lambda.c, alpha = 0)

# behavior of the coefficient as a function of log(lambda):
plot(fit.net, xvar='lambda',label=TRUE, col = rainbow(dim(x)[2])) 
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), lty=1, cex=1)

# Choice of the optimal lambda, e.g., via cross-validation:
cv.net <- cv.glmnet(x,y,lambda=lambda.c, alpha = 0, nfolds=10) # default: 10-fold CV
bestlam.net <- cv.net$lambda.min 
bestlam.net

## analogously:
# cv.net <- lm.net(y ~ reg1 + reg2, lambda = lambda.c)
# bestlam.net <- lambda.c[which.min(cv.net$GCV)]

# once you have the optimal lambda, you can re-run the model



#### Linear Regression: RIDGE solution ####

n     <- dim(data)[[1]]
y     <- data$...   #*****************************************************************************
reg1  <- data$...   #*****************************************************************************
reg2  <- data$...   #*****************************************************************************
r <- ... # number of regressors (beta0 included) #************************************************

fm <- lm(y ~ reg1 + reg2) #***********************************************************************

# check collinearity: compute Variance Inflation Factor
vif(fm)
# if reg_i has vif_i>10, it's consider high (collinear)


# build matrix of predictors:
x <- model.matrix(y~reg1+reg2+...)[,-1] #*****************************************************************************


## if you know lambda (lambda is the penalization):
lambda <- ... # bestlam.ridge #*****************************************************************************

# fitting ridge model 
fit.ridge <- glmnet(x, y, lambda = lambda, alpha = 0) # if alpha = 0: RIDGE

coef.ridge <- coef(fit.ridge) # beta coefficients
# yhat.lm <- cbind(rep(1,n), reg1, reg2)%*%coef(fm)      # LM fitted values 
yhat.r  <- cbind(rep(1,n), reg1, reg2)%*%coef.ridge    # ridge fitted values


# graphical representation: lm.ridge()    (and comparison with lm())
plot(reg1, yhat.r, type='l', lty=1, col=grey.colors(length(lambda)), lwd=2) 
points(reg1, y, pch=1, cex=.8)
# matlines(reg1, yhat.lm, type='l', lty=4, lwd=2, ylab='y',xlab='reg1') # comparison with lm
# legend("topleft",c("lm","ridge"),lty=c(4,1),col=c("black",grey.colors(length(lambda))), lwd = 2)


## if you do NOT know lambda (lambda is the penalization):
lambda.c <- seq(..., ..., length=100)  #************************************************************************
fit.ridge <- glmnet(x, y, lambda = lambda.c, alpha = 0)

# behavior of the coefficient as a function of log(lambda):
plot(fit.ridge, xvar='lambda',label=TRUE, col = rainbow(dim(x)[2])) 
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), lty=1, cex=1)

# Choice of the optimal lambda, e.g., via cross-validation:
cv.ridge <- cv.glmnet(x,y,lambda=lambda.c, alpha = 0, nfolds=10) # default: 10-fold CV
bestlam.ridge <- cv.ridge$lambda.min 
bestlam.ridge
#Coefficients of the model with best lambda:
coef.ridge <- predict(fit.ridge, s=bestlam.ridge, type = 'coefficients')[1:r,]
coef.ridge
## analogously:
# cv.ridge <- lm.ridge(y ~ reg1 + reg2, lambda = lambda.c)
# bestlam.ridge <- lambda.c[which.min(cv.ridge$GCV)]

# once you have the optimal lambda, you can re-run the model




#### Linear Regression: LASSO solution ####

n     <- dim(data)[[1]]
y     <- data$...   #*****************************************************************************
reg1  <- data$...   #*****************************************************************************
reg2  <- data$...   #*****************************************************************************
r <- ... # number of regressors (beta0 included) #************************************************

fm <- lm(y ~ reg1 + reg2) #***********************************************************************

# check collinearity: compute Variance Inflation Factor
vif(fm)
# if reg_i has vif_i>10, it's consider high (collinear)

# build matrix of predictors:
x <- model.matrix(y~reg1+reg2+...)[,-1] #*****************************************************************************


## if you know lambda (lambda is the penalization):
lambda <- ... # bestlam.lasso #*****************************************************************************




# fitting lasso model 
fit.lasso <- glmnet(x, y, lambda = lambda, alpha = 1) # if alpha = 1: LASSO

coef.lasso <- coef(fit.lasso) # beta coefficients
coef.lasso

# yhat.lm <- cbind(rep(1,n), reg1, reg2)%*%coef(fm)      # LM fitted values 
yhat.l  <- cbind(rep(1,n), reg1, reg2)%*%coef.lasso      # lasso fitted values


# graphical representation: lm.lasso()    (and comparison with lm())
plot(reg1, yhat.l, type='l', lty=1, col=grey.colors(length(lambda)), lwd=2) 
points(reg1, y, pch=1, cex=.8)
# matlines(reg1, yhat.lm, type='l', lty=4, lwd=2, ylab='y',xlab='reg1') # comparison with lm
# legend("topleft",c("lm","lasso"),lty=c(4,1),col=c("black",grey.colors(length(lambda))), lwd = 2)






## if you do NOT know lambda (lambda is the penalization):
lambda.c <- seq(..., ..., length=100) #************************************************************************
#if lambda is in a range 
lambda <- seq(0.01,1,by = 0.01)  #*******************
fit.lasso <- glmnet(x, y, lambda = lambda.c, alpha = 1)

# behavior of the coefficient as a function of log(lambda):
plot(fit.lasso, xvar='lambda',label=TRUE, col = rainbow(dim(x)[2])) 
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), lty=1, cex=1)

# Choice of the optimal lambda, e.g., via cross-validation:
cv.lasso <- cv.glmnet(x,y,lambda=lambda.c, alpha = 1, nfolds=10) # default: 10-fold CV
bestlam.lasso <- cv.lasso$lambda.min 
bestlam.lasso  #NEL GRAFICO GUARDO log(lambda)!
#Regressori e coefficienti del modello con il miglior lambda:
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[1:r,]
coef.lasso

## analogously:
# cv.lasso <- lm.lasso(y ~ reg1 + reg2, lambda = lambda.c)
# bestlam.lasso <- lambda.c[which.min(cv.lasso$GCV)]

# once you have the optimal lambda, you can re-run the model with lambda known


# PREDICTION: 
new <- data.frame(reg1 = ..., reg2 = ..., reg3 = ..., reg4 = ..., reg5 = ..., reg6 = ...) #********************************************
predict(fit.lasso, as.matrix(new), s=bestlam.lasso, interval = "prediction", level = 0.95)


#### Linear Regression: Comparison btw Elastic, Ridge, Lasso, LM ####

# Compare coefficients estimates for LS, Ridge and Lasso
plot(0,0, pch = '', axes = F, xlab = '', ylab = expression(beta), 
     xlim=c(-1,4), ylim = range(c(coef.lasso[-1], coef.ridge[-1], coef.net[-1], coef(lm(y~x))[-1])))
points(rep(0, dim(x)[2]), coef(lm(y~x))[-1], col=rainbow(dim(x)[2]), pch=20) # lm
points(rep(1, dim(x)[2]), coef.ridge[-1], col=rainbow(dim(x)[2]), pch=20)    # ridge
points(rep(2, dim(x)[2]), coef.lasso[-1], col=rainbow(dim(x)[2]), pch=20)    # lasso
points(rep(3, dim(x)[2]), coef.net[-1],   col=rainbow(dim(x)[2]), pch=20)    # elastic net
abline(h=0, col='grey41', lty=1)
box()
axis(2)
axis(1, at=c(0,1,2,3), labels = c('LS', 'Ridge', 'Lasso', 'Elastic Net'))
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), pch=20, cex=1)





#### Subset Selection: Exhaustive ####

n <- dim(data)[[1]]
y <- data$...   #*****************************************************************************
data$y <- NULL  #*****************************************************************************


# Best Subset Selection (exhaustive search): 
# I look at all the possible combinations of regressors (2K possibilities, k=number of variables):
nvmax <- 8 # show the first nvmax combinations
regfit.full <- regsubsets(y~data, nvmax = nvmax, method = 'exhaustive') 
reg.summary <- summary(regfit.full)
reg.summary
# names(reg.summary) # all possible outcome of the summary

# plotting some parameters as a function of the number of variables
par(mfrow=c(1,3))
plot(reg.summary$rsq,xlab="Number of Variables",ylab="R-squared",type="b") 
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b") 
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="b")
# choose the number of variables k such that elbows/ maximum of the R2 adj/...

k <- ... # which.max(reg.summary$adjr2) #******************************************************************************
coef(regfit.full,k) # extract coefficient estimates

# # another graphical table of best subsets:
# par(mfrow = c(1,2)) 
# plot(regfit.full,scale="r2",main="Exhaustive search") 
# plot(regfit.full,scale="adjr2",main="Exhaustive search")




#### Subset Selection: Forward ####

n <- dim(data)[[1]]
y <- data$...   #*****************************************************************************
data$y <- NULL  #*****************************************************************************


# Best Subset Selection (Forward Stepwise search): 
# In this case, I start with only one variable (the one which have max pvalue).
# At each iteration, add the variable which max pvalue of the remaining. 
# Once the variable has been chosen, he cannot be removed.
nvmax <- 8 # show the first nvmax combinations
regfit.fwd <- regsubsets(y~data, nvmax = nvmax, method = 'forward') 
reg.summary <- summary(regfit.fwd)
reg.summary
# names(reg.summary) # all possible outcome of the summary

# plotting some parameters as a function of the number of variables
par(mfrow=c(1,3))
plot(reg.summary$rsq,xlab="Number of Variables",ylab="R-squared",type="b") 
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b") 
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="b")
# choose the number of variables k such that elbows/ maximum of the R2 adj/...

k <- ... # which.max(reg.summary$adjr2) #******************************************************************************
coef(regfit.fwd,k) # extract coefficient estimates

# # another graphical table of best subsets:
# par(mfrow = c(1,2)) 
# plot(regfit.fwd,scale="r2",main="Exhaustive search") 
# plot(regfit.fwd,scale="adjr2",main="Exhaustive search")



#### Subset Selection: Backward ####

n <- dim(data)[[1]]
y <- data$...   #*****************************************************************************
data$y <- NULL  #*****************************************************************************


# Best Subset Selection (Backward Stepwise search): 
# Using the method=“backward”, you start from the last row and then remove one-at-the-time 
# (I start with the model with all the covariates, and at each step I remove one of them):
# Once the variable has been removed, he cannot be added.
nvmax <- 8 # show the first nvmax combinations
regfit.bwd <- regsubsets(y~.,data, nvmax = nvmax, method = 'backward') 
reg.summary <- summary(regfit.bwd)
reg.summary
# names(reg.summary) # all possible outcome of the summary

# plotting some parameters as a function of the number of variables
par(mfrow=c(1,3))
plot(reg.summary$rsq,xlab="Number of Variables",ylab="R-squared",type="b") 
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b") 
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="b")
# choose the number of variables k such that elbows/ maximum of the R2 adj/...

k <- ... # which.max(reg.summary$adjr2) #******************************************************************************
coef(regfit.bwd,k) # extract coefficient estimates

# # another graphical table of best subsets:
# par(mfrow = c(1,2)) 
# plot(regfit.bwd,scale="r2",main="Exhaustive search") 
# plot(regfit.bwd,scale="adjr2",main="Exhaustive search")



#### k-fold cross validation ####

# nostra super funzione:

my.subsetselection.cv <- function(Y, data, k.folds = 10, method = 'exhaustive', want.plot=T){
  # Performs 10-folds cross validation to find the optimal number
  # of variables to include in the subset selection model.
  #
  ## CALL:
  # k.cv <- subsetselection.cv(Y, data)
  #
  ## INPUT:
  # Y = vector of response variable.
  # data = dataframe of covariates.
  # k.fold = number of folds for Cross-Validation (default: k.fold=10).
  # method = method of subsect selection you want to use 
  #          possible methods: 'exhaustive', 'forward', 'backward', 'seqrep', 
  #          (default: method='exhaustive').
  # want.plot = logical input, set to T if you want a plot of cv-errors vs k,
  #           (default: want.plot=T).
  #
  ## OUTPUT:
  # k.cv= optimal number of variables that minimizes the root of mean cv-errors
  
  
  metodi = c('exhaustive', 'forward', 'backward', 'seqrep')
  if (sum(method == metodi)!=1){
    warning(paste(method), ' method not found, exaustive method has been used')
    method = 'exhaustive'
  }
  
  library(leaps) 

  n <- dim(data)[1]
  p <- dim(data)[2]
  data.all <- cbind(Y, data)
  
  set.seed(1)
  folds <- sample(1:k.folds,nrow(data),replace=TRUE) 
  
  # This is the loop that performs the estimation of the cross validation error:
  cv.errors <- matrix(NA,k.folds,p, dimnames=list(NULL, paste(1:p)))
  for(j in 1:k.folds){
    best.fit <- regsubsets(Y~.,data=data.all[folds!=j,],nvmax=p, method = method)
    for(i in 1:p){
      # funzione di merda:
      form <- as.formula('Y~.')
      mat <- model.matrix(form,data.all[folds==j,])
      coefi <- coef(best.fit,i)
      xvars <- names(coefi)
      pred <- mat[,xvars]%*%coefi

      cv.errors[j,i] <- mean( (Y[folds==j]-pred)^2 )
      }
  }

  root.mean.cv.errors <- sqrt(apply(cv.errors,2,mean)) # average over the columns

  k.cv <- which.min(root.mean.cv.errors) # minimum error

  if (want.plot==T){
    plot(root.mean.cv.errors,type='b', xlab = 'k')
    points(k.cv,root.mean.cv.errors[k.cv],
           col='red',pch=19)
  }

  return(as.numeric(k.cv))
}

k <- my.subsetselection.cv(Y, data, method = 'forward', want.plot=T)
k



#### Logistic Regression ####

## GLM regression:
# \[ g(E[y|z])=\beta_0 + \beta_1z_1 + \dots + \beta_rz_r \]
# where $g()$ is the link function.
## Logit regression is a particular GLM regression in which $y \sim Bernoulli(p)$, namely:
# \[ g(p) = log(\frac{p}{1-p}) \]
#
# other possible link functions are: 
# \[ \text{Poisson Regression:} \quad g(\lambda = log(\lambda)\]
# \[ \text{Probit Regression: } \quad g(p) = \Phi^-1(p), \quad where \Phi: \text{cumulative distr function of} N(0,1) \]

n     <- dim(data)[[1]]
y     <- data$...   #*****************************************************************************
reg1  <- data$...   #*****************************************************************************
reg2  <- data$...   #*****************************************************************************
r <- ... # number of regressors (beta0 included) #************************************************

y <- as.factor(y)

fit <- glm(y ~ reg1 + reg2, family = 'binomial') #***********************************************************************
summary(fit)


## comparing residual deviance with that of the model without regressor (i.e. mean) 
# - only if 1 regressor
plot(reg1, as.numeric(y)-1)    # plotting original points
lines(seq(range(reg1), by=0.1), predict(fit, data.frame(reg1=seq(range(reg1),by=0.1)), type='response'))
# null model
Freq.tot <- table(y)[2]/ (table(y)[1] + table(y)[2]) 
abline(h = Freq.tot, col='blue', lty=2)
# abline(h = p, col='red', lty=2) # if you have a certain proportion of y1 wrt y0

## pointwise estimate for the proportion of y for specific regressors:
predict(fit, data.frame(reg1=...), type='response')

## pointwise estimate for the specific reg1 in which y=1 exceeded y=0:
p = 0.5
(log(p/1-p)-coefficients(fit)[1])/coefficients(fit)[2]

## pointwise estimate for the specific reg1 in which the proportion of y=1 wrt y=0 is p:
p = ... #********************************************************************************
(log(p/1-p)-coefficients(fit)[1])/coefficients(fit)[2]









#### Support Vector Machine ####

library(e1071)
data$GRUPPO[which(data$GRUPPO=="NOMEGRUPPO1")] <-1
data$GRUPPO[which(data$GRUPPO=="NOMEGRUPPO2")] <-0
data$GRUPPO <- as.factor(data$GRUPPO)


# kernel='radial' (Serve settare gamma!) se i dati non sono separabili con una retta (no linear)
# Aumentando il costo aumento i support points e diminuisco gli errori di misclassificazione, ottenendo però una regione più irregolare.
svmfit <- svm(GRUPPO~ ., data=data , kernel ='linear', cost =0.1) #, gamma = ..., , scale =FALSE) 
summary(svmfit)




plot(svmfit , data, col =c('salmon', 'light blue'), pch=19, asp=1)


# support vectors (are indicated with crosses)le croci nel plot)
svmfit$index


# Misclassification Error:
table<-table(true=data[ ,"GRUPPO"], pred=predict (svmfit ,
                                                  newdata =data[,]))
err<-table[1,2]+table[2,1]
#table<-table(true=train[ ,"GRUPPO"], pred=predict (svmfit , newdata =train[,])) #Sul Trining Set


# Costo tramite CV:
#par<- list(cost=c( ..., ..., ...), gamma=c(..., ..., ...) )  #Se non lineare
#par<- list(cost=c( ..., ..., ...))                           #Se lineare
train <- # training set
tune.out <- tune(svm , y~., data=data[train ,], kernel ='...',  # in kernel 'linear' o 'radial'
                 ranges =par)
summary(tune.out)


#Best model CV:
svmfit<-tune.out$best.model


#Predizione:
newx <- matrix(c(..., ...) , ncol =2) #Dato
testdat <- data.frame(x=newx)


ypred <- predict(bestmod,testdat)
#NON Guardare il primo 1, importa solo il valore sotto! (Vuol dire che il primo dato è classificato come valore sotto)
ypred




#### Regression TREES #### (Lab_11)


attach(data)
n     <- dim(data)[1]

tree.data<- tree(NOMEVAR_Y~., data)
summary(tree.data)



# plot of the tree
plot(tree.data) 
text(tree.data,pretty=0)



## We can use cross validation to prune the tree optimally through the function cv.tree():
set.seed(...)
tree.data.cv <- cv.tree(tree.data)
# names(tree.data.cv)
# plot
plot(tree.data.cv$size,tree.data.cv$dev,type="b",xlab="size",ylab="misclass")
k <- ... # choose k that minimize misclass cost #********************************************************************************
# prune the tree
prune.data <- prune.tree(tree.data, best = k)
# plot the pruned tree
plot(prune.data) 
text(prune.data,pretty=0)





#### Classification TREES ####
attach(data)

#Se la dummy è da creare
y <- ifelse( VARY_Y <  ... ,"Classe1","Classe2")
y <- as.factor(y)
data$VAR_Y <- NULL
data <- data.frame(data, y)



tree.data<- tree(y~., data)
summary(tree.data)



# plot of the tree
plot(tree.data) 
text(tree.data,pretty=0)



## We can use cross validation to prune the tree optimally through the function cv.tree():
set.seed(...)
tree.data.cv <- cv.tree(tree.data,FUN=prune.misclass)
# names(tree.data.cv)
# plot
plot(tree.data.cv$size,tree.data.cv$dev,type="b",xlab="size",ylab="misclass")
#Guardare elbow



k <- ... # choose k that minimize misclass cost #********************************************************************************
# prune the tree
prune.data <- prune.misclass(tree.data, best = k)
# plot the pruned tree
plot(prune.data) 
text(prune.data,pretty=0)





























#### ciao ####












