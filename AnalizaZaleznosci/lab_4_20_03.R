library(dplyr)
#Zadanie 4.1
dane = read.table('airpollution.txt',header = TRUE)
#a

setdiff(dane$NOx, dane$NOxPot)
dane$NOxPot <- NULL
m1 <- lm(Mortality~., data = dane)
# to sa te same kolumny !!! ^^^^
#b
dane$NOxPot <- NULL
X <- model.matrix(m1)
y <- dane$Mortality
solve(t(X)%*%X, t(X)%*%y)
QR <- qr(X)
R <- qr.R(QR)
XX_inv <- solve(R)%*%solve(t(R))
b <- as.vector(XX_inv%*%t(X)%*%y)
m1$coef
SSE <- sum((m1$residuals)^2)
n <- nrows(X)
p <- ncol(X)
T <- b/sqrt * (SSE*diag(XX_inv)/(n-p))
summary(m1)$coef[,3]
summary(m1)$coef[,1]/summary(m1)$coef[,2]

pvals <- 2*pt(-abs(T),n-p)
summary(m1)$coef[,4]
pvals[length(pvals)]

#c
SSR <- sum((m1$fitted.values-mean(y))^2)
F <- SSR*(n-p)/(SSE*(p-1))
summary(m1)$fstatistic
1-pf(F,p-1,n-p) # p-value

# Zadanie 2
beta_0 <- 0.5
beta_1 <- 1
n <- 1000
x <- runif(n)
eps <- rnorm(n)
y <- beta_0 + beta_1 * x^2 + eps
#a
m2 <- lm(y~x)
X <- model.matrix(m2)
p <- 1
XX_inv <- solve(t(X)%*%X)
SSE <- sum((m2$residuals)^2)
b <- solve(t(X)%*%X)%*%t(X)%*%y
T <- b/sqrt(SSE*diag(XX_inv)/(n-p))
plot(x,y)

(summary(m2)$coef[2,3])^2
summary(m2)$fstatistic
