
#LISTA 9

# Zadanie 9.1

# a
data <- read.table('Miasta.txt', header = TRUE)
data_std <- scale(data)
data_std <- as.data.frame(data_std)



df <- cbind(newColName = rownames(data_std), data_std)
rownames(df) <- 1:nrow(df)
plot(df$Price~df$Work, asp = 1)
text(df$Price~df$Work, labels = df$newColName, pos = 1, xpd = NA)

# b

plot(data_std$Price~data_std$Work, asp = 1)
M.pc1 <- princomp(~.,cor = FALSE, data = data_std[,1:2])
M.pc1$loadings
abline(0,-1,col='red')
abline(0,1,col = 'blue')

# c e
M.pc <- princomp(~.,cor = FALSE, data = data_std)
M.pc$loadings
M.pc$loadings[1,3]

M.pc$scores
M.pc$scores[1,1]
sum(M.pc$loadings[,1]*data_std[1,])
 # d
summary(M.pc)
M.pc$sdev^2/sum(M.pc$sdev^2)

# e
M.pc$scores[which(M.pc$scores[,1]==max(M.pc$scores[,1]))]

biplot(M.pc,choices = 1:2)

# Zadanie 9.2
library(faraway)
View(meatspec)

# a
meat_train <- meatspec[1:172,]
meat_test <- meatspec[173:215,]

m1 <- lm(fat~.,data = meat_train)

RMSE <- 
  function(x,y)
  {
    return(sqrt(mean((y-x)^2)))
  }
RMSE(predict(m1,meat_test),meat_test$fat)

# b
n <- nrow(meat_train)
m2 <- step(m1,direction = 'backward', k = log(n))
RMSE(predict(m2,meat_test),meat_test$fat)

# c
scaled <- scale(meat_train[,-101])
means <- attr(scaled,"scaled:center")
stds <- attr(scaled,"scaled:scale")

meat_train_std <- as.data.frame(scaled)
meat_test_std <- scale(meat_test[,-101], center = means, scale = stds)

# d
M.pc <- princomp(~.,data = meat_train_std)
M.pc$loadings[,1]

# e
summary(M.pc)
plot(M.pc)
plot(1:10,M.pc$sdev[1:10],type = 'l')


D <- data.frame(cbind(meat_train$fat,M.pc$scores[,1:5]))
colnames(D)[1] <- 'fat'

comp_test <- meat_test_std%*%M.pc$loadings[,1:5]
D_test <- data.frame(cbind(meat_test$fat,comp_test))
colnames(D_test)[1] <- 'fat'

m3 <- lm(fat~.,data = D)
RMSE(predict(m3,D_test),meat_test$fat)
M.pc$loadings[1,]

beta_pcr <- m3$coef[-1]%*%t(M.pc$loadings[,1:5])/stds
beta0_pcr <- m3$coef[1]-sum(means*beta_pcr)

RMSE(beta0_pcr+as.matrix(meat_test[,-101])%*%t(as.matrix(beta_pcr)),meat_test$fat)
