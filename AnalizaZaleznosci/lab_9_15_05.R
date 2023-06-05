# LISTA 8
# Zadanie 8.1
data <- read.table('uscrime.txt', header = TRUE)
n <- nrow(data)
#a
l0 <- lm(R~.-Ex0, data = data)
summary(l0)

#b
l1 <- lm(R~ Age + Ed + Ex1 + U2 + W + X, data = data)
summary(l1)

#c
l2 <- lm(R~ Ed + Ex1, data = data)
summary(l2)  

#d
errl0 <- numeric(n)
errl1 <- numeric(n)
errl2 <- numeric(n)
for(i in 1:n)
{
  l0_cross <- lm(R~.-Ex0, data= data, subset = -i) 
  l1_cross <- lm(R~ Age + Ed + Ex1 + U2 + W + X, data = data, subset = -i)
  l2_cross <- lm(R~ Ed + Ex1, data = data, subset = -i)
  
  errl0[i] <- (data$R[i] - predict(l0_cross, data[i,]))^2
  errl1[i] <- (data$R[i] - predict(l1_cross, data[i,]))^2
  errl2[i] <- (data$R[i] - predict(l2_cross, data[i,]))^2
}

mean(errl0)
mean(errl1)
mean(errl2)

median(errl0)
median(errl1)
median(errl2)

boxplot(errl0, errl1, errl2)
boxplot(errl0 - errl1, errl2 - errl1)

# Zadanie 8.2
View(longley)

#a
m <- lm(Employed~., data = longley)
#b
X <- as.matrix(longley[,-7])
n <- nrow(X)
p <- ncol(X)
Y <- longley[,7]
mean_x <- colMeans(X)
std_x <- sqrt(apply(X,2,var)*(n-1)/n)
X_scaled <- matrix(0,nrow = n, ncol = p)
for (i in 1:p)
{
  X_scaled[,i] <- (X[,i]-mean_x[i])/std_x[i]
}

Y_cen <- Y-mean(Y)
lambda <- 10
beta_scaled <- solve(t(X_scaled)%*%X_scaled  + lambda * diag(p),t(X_scaled)%*%Y_cen)
betaRIDGE <- c(mean(Y) - mean_x%*%(beta_scaled/std_x), beta_scaled/std_x)

library(glmnet)

sd_y <- sqrt(var(Y)*(n-1)/n)
# pierwszy sposob
fit_glmnet1 <- glmnet(X,Y,alpha = 0, intercept = TRUE, standardize = TRUE, thresh = 1e-20)
coef(fit_glmnet1)
beta2 <- as.vector(coef(fit_glmnet1, s = sd_y * lambda/n, exact = TRUE, x = X, y = Y))
# drugi sposob
fit_glmnet2 <- glmnet(X,Y,alpha = 0, lambda=sd_y*lambda/n, intercept = TRUE, standardize = TRUE, thresh = 1e-20)
coef(fit_glmnet2)
beta1

# c
model_ridge <- glmnet(X,Y,alpha=0)
cv1 <- cv.glmnet(X,Y,alpha = 0, nfolds = 3)
lambda_opt <- cv1$lambda.min # tak nie
cv1$lambda.1se # tak o

beta_opt <- as.vector(coef(model_ridge, s = lambda_opt))
# lub
beta_opt <- coef(model_ridge)[,which(model_ridge$lambda == lambda_opt)]
