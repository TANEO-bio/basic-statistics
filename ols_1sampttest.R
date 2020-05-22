heightsm <- read.csv("heightsm.csv")
MH <- heightsm$Mheight
DH <- heightsm$Dheight

summary(heightsm)
sqrt(var(heightsm))
quantile(heightsm$Mheight, 0.05)
quantile(heightsm$Dheight, 0.05)
quantile(heightsm$Mheight, 0.95)
quantile(heightsm$Dheight, 0.95)

X <- cbind(rep(1,1375),MH)
beta <- solve(t(X)%*%X) %*% t(X) %*% DH

sum((DH - X %*% beta) ** 2) / (length(DH) - 2)
t(beta) %*% solve(t(X) %*% X)

hubble <- read.csv("hubble.csv")
velocity <- hubble$velocity
distance <- hubble$distance
plot(distance, velocity)

X <- distance
beta <- solve(t(X)%*%X) %*% t(X) %*% velocity

res = velocity - X %*% beta
sigma2hat = sum(res * res) / (length(res) - 2)
sqrt(sigma2hat)
plot(distance, velocity, xlab="距離(Mpc)", ylab="速度(km/s)", main="天体における距離と速度の関係")
abline(0, beta, col=3, lwd=3)

sigma_beta = sqrt(sigma2hat * solve(t(X)%*%X))

df = length(res) - 2
tbeta = qt(0.05, df)
beta
beta - tbeta * sigma_beta
beta + tbeta * sigma_beta

xx <- hubble$distance
nn <- length(xx)
beta <- 77
sigma <- 260
T <- 10000
betaest <- rep(0,T)
set.seed(20190518)
for(tt in 1:T){
  epsilon <- rnorm(nn, 0, sigma)
  yy <- beta*xx + epsilon
  betaest[tt] <- sum(xx*yy)/sum(xx^2)
}
hist(betaest)
mean(betaest)
var(betaest) * (length(betaest)-1)/length(betaest)
sqrt(sigma)
quantile(betaest, 0.05)
quantile(betaest, 0.95)
