# Zadanie 5.4
dane <- read.table('savings.txt',header = T)
# a)
m1 <- lm(Savings~dpi+ddpi+Pop15+Pop75, data = dane)
wh1 <- which(abs(rstudent(m1))>2)

X <- model.matrix(m1)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
wh2 <- which(diag(H)>2*p/n)

plot(m1,4)
wh3 <- which(cooks.distance(m1) >4/(n-p-1))
which(cooks.distance(m1)==max(cooks.distance(m1)))

dane2 <- dane[-49,]
m2 <- lm(Savings~.-Country, data = dane2)
# b)
par(mfrow=c(2,1))

plot(m2$coefficients[4]*dane2$dpi+residuals(m2)~dane2$dpi)
abline(lm(I(m2$coefficients[4]*dane2$dpi)~dane2$dpi))
# dpi nie istotna, bo prosta na wykresie jest pozioma, czyli jest to poprostu wykres residuow dla tej zmiennej !!! (p-value potwierdza)

plot(m2$coefficients[5]*dane2$ddpi+residuals(m2)~dane2$ddpi)
abline(lm(I(m2$coefficients[5]*dane2$ddpi+residuals(m2))~dane2$ddpi))

summary(m2)

#library(faraway)
#prplot(m2,3)
#prplot(m2,4)

#wygresy czesciowej regresji:
m21 <- lm(dpi~.-Savings-Country, data = dane2)
m22 <- lm(Savings~. -dpi-Country, data = dane2)
plot(m22$residuals~m21$residuals)
abline(lm(m22$residuals~m21$residuals))

m23 <- lm(ddpi~.-Savings-Country, data = dane2)
m24 <- lm(Savings~. -ddpi-Country, data = dane2)
plot(m23$residuals~m24$residuals)
abline(lm(m23$residuals~m24$residuals))

# c)
cor(dane2$Pop15,dane2$Pop75)
summary(m2, cor = TRUE)

# d)
par(mfrow = c(1,1))
plot(m2$coefficients[2]*dane2$Pop15+residuals(m2)~dane2$Pop15)
abline(lm(I(m2$coefficients[2]*dane2$Pop15+residuals(m2))~dane2$Pop15))

m3 <- lm(Savings ~. -Country, data = dane2, subset = Pop15 < 35)
m4 <- lm(Savings ~. -Country, data = dane2, subset = Pop15 >= 35)
summary(m3)
summary(m4)

#Zadanie 5.5
n <- 90
x1 <- runif(n/3,0,10)
x2 <- runif(n/3,10,20)
x3 <- runif(n/3,20,30)
x <- c(x1,x2,x3)

eps1 <- rnorm(n/3,0,1)
eps2 <- rnorm(n/3,0,3)
eps3 <- rnorm(n/3,0,5)
eps <- c(eps1,eps2,eps3)
y <- x + eps
x <- c(x,5)
y <- c(y,10)

plot(y~x)

m1 <- lm(y~x)
plot(1:(n+1),rstudent(m1))
# to naszego punktu nie wychwyca!!!

plot(m1$fitted.values,residuals(m1))
plot(m1$fitted.values,abs(residuals(m1)))

weights <- 1/lm(abs(residuals(m1))~m1$fitted.values)$fitted.values^2
m2 <- lm(y~x,weights = weights)
plot(1:(n+1),rstudent(m2))
