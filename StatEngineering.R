S <- 100000
pvals <- rep(0,S)
muT <- 150
muC <- 150
sigma <- sqrt(100)
alpha <- 0.05
set.seed(1)
nT=50
nC=50
for(ss in 1:S){
  YT <- rnorm(n=nT, mean=muT, sd=sigma)
  YC <- rnorm(n=nC, mean=muC, sd=sigma)
  delta <- mean(YT)-mean(YC)
  Z <- delta / (sqrt(1/nT + 1/nC) * sigma)
  pvals[ss] <- 1-pnorm(Z)
}
sum(ifelse(pvals < alpha, 1, 0)) / S


# 3(1)(c)
# 自分で入力する
n=ceiling((qnorm(1-0.05)+qnorm(1-0.1))**2*sigma**2/(0.25*(150-145)**2))
n
1
# 3(2)(b)
# 3(2)(a)
muT <- 150
muC <- 145
sigma <- sqrt(100)
alpha <- 0.05
beta <- 0.10
muT<-150
muC<-145
nT.vec <- 65:75
K <- length(nT.vec)
beta.vec <- rep(0,K)
set.seed(2)
for(kk in 1:K){
  nT <- nT.vec[kk]
  nC <- nT.vec[kk]
  for(ss in 1:S){
    YT <- rnorm(n=nT, mean=muT, sd=sigma)
    YC <- rnorm(n=nC, mean=muC, sd=sigma)
    delta <- mean(YT)-mean(YC)
    Z <- delta / (sqrt(1/nT + 1/nC) * sigma)
    pvals[ss] <- 1-pnorm(Z)
  }
  beta.vec[kk] <- pnorm(qnorm(1-0.05) - (muT-muC)/(sqrt(1/nT + 1/nC) * sigma))
}

plot(65:75,beta.vec, xlab="標本数",ylab="第 2 種の過誤確率")
abline(h=0.1,lty=2)
# 続きは自分で入力する


# 3(2)(c)
# 自分で入力する


# 3(3)(a-b)：（正規分布，分散未知)
nT.vec <- 65:75
K <- length(nT.vec)
beta.vec <- rep(0,K)
set.seed(2)
for(kk in 1:K){
  print(kk)
  nT <- nT.vec[kk]
  nC <- nT.vec[kk]
  for(ss in 1:S){
    YT <- rnorm(n=nT, mean=muT, sd=sigma) # 試験群の観測値
    YC <- rnorm(n=nC, mean=muC, sd=sigma) # 対照群の観測値
    #delta <- mean(YT)-mean(YC) # 不要な計算なのでコメントアウト
    t.tmp <- t.test(YT, YC, alternative="greater")
    pvals[ss] <- t.tmp$p.value
  }
  beta.vec[kk] <- prop.table(table(pvals < alpha))[1] # 第2種の過誤確率
  print("done")
}
plot(nT.vec,beta.vec,xlab="Sample size (per group)",ylab="Type II error")
abline(v=70,h=0.1,lty=2)	
