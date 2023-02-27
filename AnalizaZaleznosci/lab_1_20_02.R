#install.packages("dplyr")
library(dplyr)
# Zadanie 1.1
#a
print(str(daneSoc))
#b
wykszt <- daneSoc %>% pull(wyksztalcenie)
praca <- daneSoc %>% pull(praca)
print(table(wykszt))
print(table(praca))
#c
grupa <- daneSoc %>% filter(wyksztalcenie == 'srednie') %>% 
  select(`cisnienie skurczowe`)
summary(grupa)
#d
mez_zatr <- daneSoc %>% filter(praca != 'nie pracuje' & plec == 'mezczyzna') %>% 
  select(`cisnienie skurczowe`)
mez_n_zatr <- daneSoc %>% filter(praca == 'nie pracuje'& plec == 'mezczyzna') %>% 
  select(`cisnienie skurczowe`)

boxplot(daneSoc[daneSoc$plec == 'mezczyzna',]$`cisnienie skurczowe` ~ daneSoc[daneSoc$plec == 'mezczyzna',]$praca)

#e
daneSoc %>% 
  filter(wyksztalcenie == 'srednie' 
         & `cisnienie skurczowe` <= 150 & `cisnienie skurczowe` >= 140) 
#f
daneSoc %>% 
  filter(`cisnienie skurczowe` == max(daneSoc$`cisnienie skurczowe`))
#g
daneSoc %>% 
  filter(`cisnienie skurczowe` > quantile(daneSoc$`cisnienie skurczowe`,na.rm = T, probs = 0.8))

# Zadanie 1.2
#a
n1 <- rnorm(10)
qqnorm(n1)
qqline(n1, col = 'red')

n2 <- rnorm(500)
qqnorm(n2)
qqline(n2, col = 'red')
#b
g1 <- rgamma(10,2,2)
qqnorm(g1)
qqline(g1, col = 'blue')

g2<- rgamma(500,2,2)
qqnorm(g2)
qqline(g2, col = 'blue')
#c
c1 <- rcauchy(10)
qqnorm(c1)
qqline(c1, col = 'yellow')

c2 <- rcauchy(500)
qqnorm(c2)
qqline(c2, col = 'yellow')

# Zadanie 1.3
probka <- read.table('skorelowana_probka.txt',header = TRUE)
head(probka)
#a
plot(probka$x, probka$y)
#b
x <- probka$x
y <- probka$y
korelacja <- sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))**2)*sum((y-mean(y))**2))
korelacja
cor(probka$x,probka$y)
#c
n <- length(x)
2*(1-pnorm(sqrt(n)*korelacja,0,1))
#d
tanh(atanh(korelacja) + qnorm(0.975)/sqrt(n))
tanh(atanh(korelacja) - qnorm(0.975)/sqrt(n))

tanh(atanh(korelacja) +c(-1,1)*qnorm(0.975)/sqrt(n))
#e
# spearmana z def:
xr <- rank(x)
yr <- rank(y)
cor(xr, yr)
plot(xr,yr)

#f
cor(x,y,method = 'spearman')
cor(x,y,method = 'kendall')
# kendalla z def:
zl <- 0
for(i in 1:n)
{
  for(j in i:n)
  {
    zl <- zl + sign((x[i]-x[j])*(y[i]-y[j]))
  }
}
zl <- zl/(n*(n-1)/2)
zl
#g
# przyklad gdy mamy powtarzajace sie wartosci
xl <- c(x,x[n])
yl <- c(y,y[n])
rank(xl)
rank(yl)
