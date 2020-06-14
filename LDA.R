Salmon <- read.csv("salmon_ex2.csv")
Salmon
salmonA <- Salmon[1:50,3:4]
salmonC <- Salmon[51:100,3:4]
mu_a <- c(mean(salmonA$Freshwater), mean(salmonA$Marine))
mu_c <- c(mean(salmonC$Freshwater), mean(salmonC$Marine))
sigma <- var(Salmon[,c(3,4)])
variance <- function(x) var(x)*(length(x[,1])-1)/length(x[,1])
variance(Salmon[,c(3,4)])
sigma
ab <- (mu_a + mu_c)*0.5
gh <- mu_a - mu_c
plot(Salmon[,c(3,4)], pch=ifelse(Salmon$AorC=="Alaska", "A", "C"))
omega[1,] %*% gh
omega[2,] %*% gh
-gh[1]*(ab%*%omega[,1]) -gh[2]*(ab%*%omega[,2])
a <- omega[1,] %*% gh/omega[2,] %*% gh * -1
b <- (-gh[1]*(ab%*%omega[,1]) -gh[2]*(ab%*%omega[,2])) / omega[2,] %*% gh * -1
abline(b, a)

pred <- ifelse(Salmon$Marine - as.vector(Salmon$Freshwater %*% a + seq(b,b,length=length(Salmon[1,])))>=0, "Alaska", "Canada")
true <- Salmon$AorC
tab <- table(true, pred)
tab
tab[2]/sum(tab)
tab[3]/sum(tab)
tab[3]/(tab[1]+tab[3])
tab[2]/(tab[2]+tab[4])

library(rrcov)
data(hemophilia)
X1 <- hemophilia[hemophilia$gr=="carrier",1:2]
X2 <- hemophilia[hemophilia$gr=="normal",1:2]
N1 <- dim(X1)[1]
N2 <- dim(X2)[1]
mu1 <- apply(X1,2,sum)/N1
mu2 <- apply(X2,2,sum)/N2
sigma <- var(hemophilia[,c(1,2)])
N1/(N1+N2)
N2/(N1+N2)

sigma

ab <- (mu1 + mu2)*0.5
gh <- mu1 - mu2
omega <- solve(sigma)
omega[1,] %*% gh
omega[2,] %*% gh
-gh[1]*(ab%*%omega[,1]) -gh[2]*(ab%*%omega[,2])
a <- omega[1,] %*% gh/omega[2,] %*% gh * -1
b <- (-gh[1]*(ab%*%omega[,1]) -gh[2]*(ab%*%omega[,2])) / omega[2,] %*% gh * -1
c(a,b)
fisher_LDA <- function(x) (omega[1,] %*% gh) * x[1] + (omega[2,] %*% gh) * x[2] -gh[1]*(ab%*%omega[,1]) -gh[2]*(ab%*%omega[,2])
pred <- as.vector(ifelse(apply(hemophilia[,1:2],1,fisher_LDA)<0,"normal","carrier"))
true <- hemophilia$gr
true
tab <- table(true, pred)
tab

bayes_LDA <- function(x) (omega[1,] %*% gh) * x[1] + (omega[2,] %*% gh) * x[2] -gh[1]*(ab%*%omega[,1]) -gh[2]*(ab%*%omega[,2]) + log(N1/N2)
pred <- as.vector(ifelse(apply(hemophilia[,1:2],1,bayes_LDA)<0,"normal","carrier"))
true <- hemophilia$gr
true
tab <- table(true, pred)
tab

new_LDA <- function(x) (omega[1,] %*% gh) * x[1] + (omega[2,] %*% gh) * x[2] -gh[1]*(ab%*%omega[,1]) -gh[2]*(ab%*%omega[,2]) + min(apply(X1,1,fisher_LDA))
pred <- as.vector(ifelse(apply(hemophilia[,1:2],1,new_LDA)<0,"normal","carrier"))
true <- hemophilia$gr
tab <- table(true, pred)
tab

