# Zadanie 1
dane <- read.table('airpollution.txt', header = T)
# a)
m1 <- lm(dane$Mortality~dane$NOx)

m1$coefficients

summary(m1)
plot(dane$NOx, dane$Mortality)
abline(m1) # no takie srednie to dopasowanie
# b)
m2 <- lm(dane$Mortality~log(dane$NOx))
summary(m2)
plot(log(dane$NOx), dane$Mortality)
abline(m2)
# c)
plot(m2,1) # wykres residuow
residuals(m2) # zwykle
rstandard(m2) # studentyzowane
rstudent(m2) # studentyzowane modyfikowane

r2 <- rstudent(m2)
r2[abs(r2) > 2]
wh <- which(abs(r2)>2)

m3 <- lm(dane$Mortality~log(dane$NOx),subset= -wh)
summary(m3)
plot(log(dane$NOx), dane$Mortality)
abline(m2, col = 'blue')
abline(m3, col = 'red')

# Zadanie 2
# a)
phila <- read.table('phila.txt',header = T)
phila <- phila[!is.na(phila$CrimeRate),]
# b)
m1 <- lm(phila$HousePrice~phila$CrimeRate)
plot(phila$CrimeRate, phila$HousePrice)
abline(m1)
summary(m1)

r2 <- rstudent(m1)
r2[abs(r2) > 2]

# dwie metody na znalezienie odstajacej
wh <- which(abs(r2)>2)
# ta pod jest troche na chama
sub <- which(phila$CrimeRate > 300)

# znajdowanie obserwacji wplywowych
X <- model.matrix(m1)
which(hatvalues(m1) > 2 * ncol(X)/nrow(X))

which(cooks.distance(m1)>4/(nrow(X)-ncol(X)-1))
#obie metody wskazuja na obserwacje wplywowa!

m2 <- lm(phila$HousePrice~phila$CrimeRate, subset = -63)
plot(phila$CrimeRate, phila$HousePrice)
abline(m1)
abline(m2, col = 'red')
summary(m2)

# Zadanie 3

cel <- read.table('cellular.txt',header = T)
plot(cel$Period,cel$Subscribers)
m <- lm(cel$Subscribers~cel$Period)
summary(m)
abline(m)

cel <- read.table('cellular.txt',header = T)
plot(cel$Period,log(cel$Subscribers))
m1 <- lm(log(cel$Subscribers)~cel$Period)
summary(m1)
abline(m1)

cel <- read.table('cellular.txt',header = T)
plot(cel$Period,(cel$Subscribers)^(1/2))
m1 <- lm((cel$Subscribers)^(1/2)~cel$Period)
summary(m1)
abline(m1)

cel <- read.table('cellular.txt',header = T)
plot(cel$Period,(cel$Subscribers)^(1/4))
m1 <- lm((cel$Subscribers)^(1/4)~cel$Period)
summary(m1)
abline(m1)

library(MASS)
boxcox(m, lambda = seq(0,1,0.01))
# lambda = 0.2 jests ok!

cel <- read.table('cellular.txt',header = T)
plot(cel$Period,(cel$Subscribers)^(1/5))
m1 <- lm((cel$Subscribers)^(1/5)~cel$Period)
summary(m1)
abline(m1)
