library(dplyr)
# Zadanie 3.1
dane <- read.table('realest.txt',header = TRUE)
m1 <- lm(Price~.,data = dane)
#a)
X <- dane %>% 
  select(!Price)
Y <- dane %>% 
  select(Price)

X <- cbind(1,X)

X <- data.matrix(X)
Y <- data.matrix(Y)
#b
beta <- solve(t(X)%*%X)%*%t(X)%*%Y
coef(m1)

SST <- sum((Y-mean(Y))^2)
SSR <- sum((m1$fitted.values - mean(Y))^2)
SSE <- sum((m1$fitted.values - Y)^2)

wsp_det <- 1-SSE/SST
# lub
SSR/SST

#c
m1$coef
m2 <- lm(Price~Bedroom, data = dane)
m2$coef

#d
predict(m1,newdata=data.frame(
  Bedroom = 3,
  Space = 1500,
  Room = 8,
  Lot = 40,
  Bathroom = 2,
  Garage = 1,
  Tax = 1000,
  Condition = 0))
#lub
c(1,3,1500,8,40,1000,2,1,0)%*%m1$coef
#e
est_war <- SSE/17
#albo
n <- nrow(X)
p <- ncol(X)
SSE/(n-p)


# Zadanie 2
#A na tablicy
#b
n <- 100
x_1 <- rnorm(n)
x_2 <- rnorm(n)
x_3 <- rnorm(n)
eps <- rnorm(n)
beta_0 <- 2
beta_1 <- 1/2
beta_2 <- 1
beta_3 <- 7/10
y <- beta_0 + beta_1*x_1 + beta_2*x_2 + beta_3*x_3 + eps
dane <- data.frame(y = y, a = x_1, b = x_2, c = x_3)
m1 <- lm(y~., data = dane)
sum(m1$residuals)

#c
residuals <- rep(0,1000)
for(i in 1:1000){
  n <- 100
  x_1 <- rnorm(n)
  x_2 <- rnorm(n)
  x_3 <- rnorm(n)
  eps <- rnorm(n,0,10)
  beta_0 <- 2
  beta_1 <- 1/2
  beta_2 <- 1
  beta_3 <- 7/10
  y <- beta_0 + beta_1*x_1 + beta_2*x_2 + beta_3*x_3 + eps
  dane <- data.frame(y = y, a = x_1, b = x_2, c = x_3)
  m1 <- lm(y~., data = dane)

  residuals[i] <- summary(m1)$sigma^2
}
mean(residuals)
