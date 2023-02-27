library(dplyr)
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
