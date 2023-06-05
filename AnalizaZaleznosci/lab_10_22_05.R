#Zadanie 8.3
data <- read.table('prostate.txt', header = TRUE)
# a
m1 <- lm(lpsa~. -train, data = data)
m2 <- step(m1, direction = 'backward', k = 2)
m3 <- step(m2, direction = 'backward', k = log(n))

summary(m1)$r.squared
summary(m2)$r.squared
summary(m3)$r.squared

summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
summary(m3)$adj.r.squared

# b
library(glmnet)
X <- model.matrix(m)
Y <- as.vector(data$lpsa)
model_lasso <- glmnet(X,Y,alpha = 1, intercept = TRUE)
n_lambdas <- ncol(coef(model_lasso))
matplot(n_lambdas:1,t(coef(model_lasso))[,-1], type = 'l')

cv1 <- cv.glmnet(X,Y,alpha = 1, nfolds = 5)
cv1$lambda.min
cv1$lambda.1se

model_lasso2 <- glmnet(X,Y,alpha = 1, lambda = cv1$lambda.1se)
coef(model_lasso2)

# Zadanie 8.4
# z wykladu !!!

# Zadanie 8.5
library(matrixcalc)
lambda <- 1000
p <- 10
n <- 100
L <- 1000

beta <- rep(1,10)

beta_lm <- matrix(numeric(L*p), nrow = p)
beta_ridge <- matrix(numeric(L*p), nrow = p)

for(l in 1:L)
{
  x <- matrix(rnorm(n*p), nrow = n)
  eps <- rnorm(n)
  y <- x%*%beta + eps
  beta_lm[,l] <- lm(y~x-1)$coef
  beta_ridge[,l] <- as.vector(glmnet(x=x, y=y, alpha=0, intercept=FALSE, lambda=lambda)$beta)
}

bias <- function(x){return(x-beta)}
apply(apply(beta_lm,2,bias),1,mean)
apply(apply(beta_ridge,2,bias),1,mean)

is.positive.semi.definite(cov(t(beta_lm)) - cov(t(beta_ridge)))

#LISTA 9

# Zadanie 9.1
