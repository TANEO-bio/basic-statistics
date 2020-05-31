hubble <- read.csv("hubble.csv")
xx <- hubble$distance
yy <- hubble$velocity
nn <- length(xx)
beta <- 77 # βの真値を設定
sigma <- 260 # σの真値を設定
bunbo <- sqrt(sum(xx^2))
sigbeta <- sigma/bunbo
tupper <- qt(0.95, (nn-1)) # t 分布の上側 2.5%点(自由度:nn-1)
kk <- 10000 # 繰り返し回数を 10000 回と設定
betaest <- matrix(0, kk, 3) # 回帰係数推定値，信頼区間下限・上限を保存する行列を準備
set.seed(529)

for(i in 1:kk){
  epsilon <- rnorm(nn, 0, sigma) # 平均 0，標準偏差 sigma の正規乱数を nn 個生成
  yy <- beta*xx + epsilon
  betaest[i,1] <- sum(xx*yy)/sum(xx^2)
  sigest <- sqrt(sum((yy-betaest[i,1]*xx)^2)/(nn-1))
  sigbetaest <- sigest/bunbo
  betaest[i,2] <- betaest[i,1]-tupper*sigbetaest # 信頼下限
  betaest[i,3] <- betaest[i,1]+tupper*sigbetaest # 信頼上限
}
kks <- 100
pchs<- ifelse((betaest[1:kks,2] < beta)&(betaest[1:kks,3] > beta),NA,16)
cols <- ifelse((betaest[1:kks,2] < beta)&(betaest[1:kks,3] > beta),"black","red")
plot(c(1,1), c(betaest[1,2], betaest[1,3]),type='n', ylim=range(betaest[1:kks,]), xlim=c(0,kks),xlab='Iteration',ylab='90% confidence interval',cex.lab=1.3)
for(i in 1:kks){
  lines(c(i,i), c(betaest[i,2],betaest[i,3]),lwd=2,col=cols[i])
  points(c(i,i), c(betaest[i,2],betaest[i,3]),pch=pchs[i],col=cols[i])
}
abline(h=beta,col='blue',lwd=2)
sum(((betaest[,2] < beta)&(betaest[,3] > beta))==FALSE)

snow <- read.table("snowsnow.txt", sep=",")
early <- snow$Early
late  <- snow$Late
X <- cbind(rep(1, length(early)), early)
y <- late
beta <- solve(t(X) %*% X) %*% t(X) %*% y
beta
e_hat = y - X %*% beta
sigma2 = sum(e_hat ** 2) / (length(e_hat)-2)
sigma_beta_2 = sigma2 * solve(t(X) %*% X)
df = length(e_hat) - 2
tdf = qt(0.025, 91)
beta1_25 = beta[2] - tdf * sqrt(sigma_beta_2[4])
beta1_975 = beta[2] + tdf * sqrt(sigma_beta_2[4])
beta1_25
beta1_975

alpha <- 0.1
S <- 10000
beta.est<- numeric(S)
sigma.est <-  numeric(S)
stats  <- numeric(S)
xtx <- sum(xx^2)
q.t <- qt(1-alpha/2, df=nn-1)
beta <- 0; sigma <- 260
set.seed(20200529)
for(ss in 1:S){
  epsilon <- rnorm(n=nn, mean=0, sd=sigma)
  yy <- beta*xx + epsilon
  beta.est[ss] <- sum(xx*yy)/sum(xx^2)
  residual <- yy - beta.est[ss]*xx
  sigma.est[ss] <- sqrt(sum(residual^2)/(nn-1))
  sd.beta.est <- sigma.est[ss]/sqrt(xtx)
  stats[ss] <- beta.est[ss]/sd.beta.est
}
reject <- ifelse(abs(stats)>q.t,1,0)
sum(reject)          
