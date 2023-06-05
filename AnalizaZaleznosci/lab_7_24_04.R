# Lista 6
# Zadanie 6.1
dane <- longley
# a)
m1 <- lm(Employed~., data = dane)
# b)
library(ppcor)
X <- model.matrix(m1)
pcor(X)
R2 <- numeric(ncol(X))
for( i in 1:ncol(X)){
  m <- lm(X[,i]~X[,-i], data = as.data.frame(X))
  R2[i] <- summary(m)$r.squared
}
VIF <- 1/(1-R2)
VIF
colnames(X)

# Zadanie 6.2
# a)
dane <- read.table('uscrime.txt',header = T)
m1 <- lm(R~., data = dane)
X <- model.matrix(m1)

pairs(dane[,-1])
round(cor(dane[,-1]),3)
abscor <- abs(round(cor(dane[,-1]),3))
max(abscor-diag(rep(1,ncol(abscor))))

m2 <- lm(R~.-Ex0, data = dane)
summary(m1)
summary(m2)

# b)
n <- nrow(model.matrix(m2))
p <- ncol(model.matrix(m2))
SSE <- sum((residuals(m2))^2)

# AIC
AIC(m2)
-2 * logLik(m2) + 2*(p+1)
n*log(SSE/n)+2*p

#BIC
BIC(m2)
AIC(m2,k=log(n))
-2*logLik(m2) + log(n)*(p+1)
n*log(SSE/n) + log(n)*p

#Adj r2
summary(m2)
summary(m2)$adj.r.squared
1-(1-summary(m2)$r.squared)*(n-1)/(n-p)


# Mallows
SSE + summary(m2)$sigma^2*p*2

col_names <- colnames(dane)[c(-1,-5)]
formulas <- 
  lapply(
apply(expand.grid(rep(list(c(FALSE,TRUE)),p-1)),1,
      function(x){return(col_names[x])}),
function(x){return(paste0('R~',paste0(x,collapse = '+')))}
)
formulas[[1]] <- 'R~1'

sigma2 <- summary(m2)$sigma^2

wyniki <- data.frame(formula_lm = numeric(length(formulas)),
                     AIC = numeric(length(formulas)),
                     BIC = numeric(length(formulas)),
                     adjr2 = numeric(length(formulas)),
                     Mallows = numeric(length(formulas))
                     )

for(i in 1 : length(formulas)){
  f <- as.formula(formulas[[i]])
  m <- lm(f,data = dane)
  X <- model.matrix(m)
  pf <- ncol(X)
  SSE <- sum(residuals(m)^2)
  wyniki$formula_lm[i] <- formulas[[i]]
  wyniki$AIC[i]<-n*log(SSE/n)+2*pf
  wyniki$BIC[i] <- n*log(SSE/n)+log(n)*pf
  wyniki$adjr2[i] <- -1-(1-summary(m)$r.squared)*(n-1)/(n-pf)
  wyniki$Mallows[i] <- SSE+sigma2*(pf*2)
}
# c)
step(m2,direction = 'backward', k = 2) #k = 2 <- znaczy ze uzywamy funkcji akaike
m_null <- lm(R~1, data = dane)
step(m_null, scope = list(lower = m_null, upper = m2), direction = 'forward',k=2)
step(m2,direction = 'both',k=2)


wyniki$formula_lm[wyniki$AIC == min(wyniki$AIC)]
wyniki$formula_lm[wyniki$BIC == min(wyniki$BIC)]
wyniki$formula_lm[wyniki$adjr2 == min(wyniki$adjr2)]
wyniki$formula_lm[wyniki$Mallows == min(wyniki$Mallows)]

#d
rank(abs(summary(m2)$coef[-1,3]))
L<-list()

L[[1]]<-lm(R~Ex1,data = Data)
L[[2]]<-lm(R~Ex1+X,data = Data)
L[[3]]<-lm(R~Ex1+X+Age,data = Data)
L[[4]]<-lm(R~Ex1+X+Age+Ed,data = Data)
L[[5]]<-lm(R~Ex1+X+Age+Ed+U2,data = Data)
L[[6]]<-lm(R~Ex1+X+Age+Ed+U2+U1,data = Data)
L[[7]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W,data = Data)
L[[8]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M,data = Data)
L[[9]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S,data = Data)
L[[10]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S+N,data = Data)
L[[11]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S+N+LF,data = Data)
L[[12]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S+N+LF+NW,data = Data)

which_min_AIC<-which(sapply(L,AIC)==min(sapply(L,AIC)))
L[[which_min_AIC]]
which_min_BIC<-which(sapply(L,BIC)==min(sapply(L,BIC)))
L[[which_min_BIC]]

#Zad 6.3



L<-50
p<-9
betas<-c(1,1,1,0,0,0,0,0,0)

N<-c(25, 50, 75, 100, 125, 150, 175, 200)
probs_mean_AIC<-numeric(length(N))
probs_mean_BIC<-numeric(length(N))
for(n in N)
{
  probsAIC<-numeric(L)
  probsBIC<-numeric(L)
  
  for(i in 1:L)
  {
    X<-matrix(rnorm(n*p),ncol = p)
    eps<-rnorm(n)
    y<-X%*%betas+eps
    d<-data.frame(X,y)
    m<-lm(y~.,data = d)
    mAIC<-step(m,direction = "backward",trace = FALSE,k=2)
    mBIC<-step(m,direction = "backward",trace = FALSE,k=log(n))
    probsAIC[i]<-setequal(names(coef(mAIC))[-1], c("X1","X2","X3"))
    probsBIC[i]<-setequal(names(coef(mBIC))[-1], c("X1","X2","X3"))
  } 
  
  probs_mean_AIC[which(N==n)] <- mean(probsAIC)
  probs_mean_BIC[which(N==n)] <- mean(probsBIC)
  
}

plot(probs_mean_AIC~N,type = 'b',col = 'magenta',ylim = c(0,1))
lines(probs_mean_BIC~N,type = 'b',col = 'cyan')
