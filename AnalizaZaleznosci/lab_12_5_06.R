# LISTA 10

# Zadanie 10.1
data <- read.table('SAheart.data',sep = ',',header = T)
data <- data[,-1]

# a
m1 <- glm(chd~., data = data, family = 'binomial')

# b
summary(m1)
#bonus
predict(m1) # X\hat{\beta}
#hat{p}
exp(predict(m1))/(1+exp(predict(m1)))
predict(m1,type = 'response') #hat{p}
# c

exp(m1$coefficients['age'])

# d
n <- nrow(data)
m_aic <- step(m1, direction = 'backward')
m_bic <- step(m1, direction = 'backward',  k = log(n))

# e

sum_m1 <- summary(m1)

sum_m1$null.deviance-sum_m1$deviance

1-pchisq(sum_m1$null.deviance-sum_m1$deviance, sum_m1$df.null - sum_m1$df.residual)
# model ma conajmniej jedna istotna zmienna

# f

m2 <- glm(chd~.+I(age^2), data = data, family = 'binomial')
sum_m2 <- summary(m2)
1-pchisq(sum_m1$deviance-sum_m2$deviance, sum_m1$df.residual - sum_m2$df.residual)

# zad 10.2

eq <- read.table('earthquake.txt', header = TRUE)
eq$popn <- as.factor(eq$popn)
m1 <- glm(popn~., data = eq, family = 'binomial')
meq <- glm(popn~., data = eq, family = 'binomial')
summary(meq)

library(ggplot2)
ggplot(data = eq, aes(x = body, y = surface, col = popn)) + geom_point(size = 2.5)





