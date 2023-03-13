library(ppcor)
library(dplyr)
library(ggplot2)
library(MASS)
# Zadanie 2.1
dane <- read.table('airpollution.txt', header = TRUE)
#a
mor <- dane$Mortality
edu <- dane$Education
cor1 <- cor(mor,edu)
#b
k <- 100000
corrs <- numeric(k)
for(i in 1:k)
{
  morper <- sample(mor)
  corrs[i] <- cor(morper,edu)
}
#c
hist(corrs)
abline(v = cor1, col = 'red')
#d
(1+sum(abs(corrs)>abs(cor1)))/(1+length(corrs))
#e

# Zadanie 2.2
#a b
z <- rnorm(10000)
n_x <- rnorm(10000)
n_y <- rnorm(10000)
x <- 2*z + n_x
y <- -5*z + n_y

cor(x,y)
-10/sqrt(5*26)

ramka_xyz <- data.frame(z,x,y)
pcor(ramka_xyz)
#b inaczej
m1 <- lm(x~z-1)
m1$coef

m2 <- lm(y~z-1)
m2$coef

cor(x-m1$coef*z,y-m2$coef*z)

#c
n_v <- rnorm(10000)
v <- z^2 + n_v
m3 <- lm(v~z-1)
m3$coef

cor(v-m3$coef*z, y-m2$coef*z)

# Zadanie 3
eps <- rnorm(101,0,3)
x <- seq(0,10,0.1)
y = x + eps
plot(x,y,col = 'blue')

#a
cov(x,y)/sqrt(var(x)*var(y))
cor(x,y)
#b
model <- lm(y ~ x)
model$coef
#c
abline(model$coef[1], model$coef[2], col = 'red')
#d
eps <- rnorm(101,0,0.5)
x <- seq(0,10,0.1)
y = x + eps
plot(x,y,col = 'blue')
cov(x,y)/sqrt(var(x)*var(y))
cor(x,y)
model <- lm(y ~ x)
model$coef
abline(model$coef[1], model$coef[2], col = 'red')

b1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
b0 <- mean(y)-b1*mean(x)

#Zadanie 4
hills
#a
par(mfrow=c(2,1))

#b
model_td <- lm(hills$dist ~hills$time)
model_tc <- lm(hills$climb ~hills$time)

plot(hills$time, hills$dist)
abline(model_td,col = 'blue')

plot(hills$time, hills$climb)
abline(model_tc,col = 'blue')

SST <- sum((hills$time-mean(hills$time))^2)
SSR_mtd <- sum((model_td$fitted.values-mean(hills$time))^2)
SSR_mtc <- sum((model_tc$fitted.values-mean(hills$time))^2)
SSE_mtd <- sum((model_td$fitted.values-hills$time)^2)
SSE_mtc <- sum((model_tc$fitted.values-hills$time)^2)

sum(model_td$residuals^2)
SSE_mtd

SSR_mtd/SST
SSR_mtc/SST

summary(model_td)

#c
sum(model_td$coef*c(1,15))
predict(model_td, data.frame(dist = 15))

# Zadanie 2.5
kwartet <- read.table('anscombe_quartet.txt',header = TRUE)
