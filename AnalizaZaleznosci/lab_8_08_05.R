# lista 6 kontynuacja
# Zadanie 7.1
View(trees)
m1 <- lm(Volume~Girth, data = trees)
m2 <- lm(Volume~Girth + Height, data = trees)
m3 <- lm(Volume~Girth + Height + I(Height*Height), data = trees)
summary(m3)

# a)
n <- nrow(trees)

SSE_1 <- sum((m1$residuals)^2)
SSE_2 <- sum((m2$residuals)^2)
F_1_2 <- (SSE_1 - SSE_2)/(SSE_2/(n-3))
p_value_1 <- 1 - pf(F_1_2, 1, n-3)
anova(m1,m2)


SSE_3 <- sum((m3$residuals)^2)
F_2_3 <- (SSE_2 - SSE_3)/(SSE_3/(n-4))
p_value_2 <- 1 - pf(F_2_3, 1, n-4)
anova(m2,m3)


# b)
anova(m1,m3)

#Zadanie 7.2
# a)
library(car)

mod.davis <- lm(weight ~ repwt, data=Davis)

## the following are equivalent:
linearHypothesis(mod.davis, diag(2), c(0,1))
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
linearHypothesis(mod.davis, c("(Intercept)", "repwt"), c(0,1))
linearHypothesis(mod.davis, c("(Intercept)", "repwt = 1"))

1 - pf(1.7353, 2 ,181)


mod.duncan <- lm(prestige ~ income + education, data=Duncan)

## the following are all equivalent:
linearHypothesis(mod.duncan, matrix(c(0,-1,1),nrow = 1),0)
linearHypothesis(mod.duncan, "1*income - 1*education = 0")
linearHypothesis(mod.duncan, "income = education")
linearHypothesis(mod.duncan, "income - education")
linearHypothesis(mod.duncan, "1income - 1education = 0")
linearHypothesis(mod.duncan, "0 = 1*income - 1*education")
linearHypothesis(mod.duncan, "income-education=0")
linearHypothesis(mod.duncan, "1*income - 1*education + 1 = 1")
linearHypothesis(mod.duncan, "2income = 2*education")

# b)
Data <- read.table('ExerciseCholesterol.txt', header = T)
X <- as.matrix(Data)
n <- nrow(X)
mat <- matrix(c(rep(1,8),rep(0,18),rep(0,8),rep(1,8),rep(0,9),rep(0,17),rep(1,9)),nrow = 26)
Weight1 <- numeric(n)
Weight2 <- numeric(n)
Weight3 <- numeric(n)

Weight1[Data$Group==1] <- Data$Weight[Data$Group==1]

Weight2[Data$Group==2] <- Data$Weight[Data$Group==2]

Weight3[Data$Group==3] <- Data$Weight[Data$Group==3]

Data_new <- data.frame(mat,Weight1, Weight2, Weight3, HDL = Data$HDL)

m <- lm(HDL~.-1, data = Data_new)
C <- matrix(c(0,0,0,1,-1,0,0,0,0,0,1,-1), nrow=2, byrow = T)
d <- c(0,0)
linearHypothesis(m,C,d)
