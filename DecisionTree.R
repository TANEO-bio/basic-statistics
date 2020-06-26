#part1
library(rpart)
data(kyphosis)
tree.k1 <- rpart(Kyphosis~., data=kyphosis)
par(xpd=TRUE)
plot(tree.k1)
text(tree.k1, use.n=TRUE)

tree.k2 <- rpart(Kyphosis~., data=kyphosis, parms=list(split="information"))
par(xpd=TRUE)
plot(tree.k2)
text(tree.k2, use.n=TRUE)

#part2
library(rrcov)
data(hemophilia)

split.gini <- function(x, g, Nmin=3) {
  N <- length(x)
  Nmax <- N - Nmin
  g <- g[order(x)]
  x <- sort(x)
  y <- ifelse(g=="carrier", 1, 0)
  G1num <- cumsum(y)
  bunki <- rep(0, N) # 分岐点の値を格納するベクトル
  p.left <- p.right <- rep(-1, N) # 保有者群の割合を格納するベクトル
  gini  <- rep(1, N) # 分岐点に対応するジニ係数の値を格納するベクトル
  for(ii in Nmin:Nmax) {
    if(x[ii] != x[ii+1]){ 
      bunki[ii]   <- (x[ii]+x[ii+1])/2
      p.left[ii]  <- G1num[ii]/ii
      p.right[ii] <- 1 - G1num[ii]/ii
      gini.left   <- 1-p.left[ii]^2-(1-p.left[ii])^2
      gini.right  <- 1-p.right[ii]^2-(1-p.right[ii])^2
      gini[ii]    <- (ii/N)*gini.left + ((N-ii)/N)*gini.right
    }
  }
  return(cbind(bunki, gini, p.left, p.right, N.left=1:N, N.right=(N-1):0))
}

split_func <- function(x,y){
  for i in range(1:dim(x)[2]){
    res <- data.frame(split.gini(x[,i],y))
  }
  bunki <- res[which.min(res$gini),]$bunki
  x1 <- x[x > res$bunki]
  x2 <- x[x <= res$bunki]
  y1 <- y[x > res$bunki]
  y2 <- y[x <= res$bunki]
  if (length(x) < 10) {
    return (bunki)
  }
  if (length(x1) <= 3){
    return(bunki)
  }
  if (length(x2) <= 3) {
    return (bunki)
  }
    return (list(split_func(x1, y1), split_func(x2, y2)))
}

DecisionTreeClassifier <- function(x, y, min_samples=3, min_leaves=10){
  split_func(x,y)
}

res = DecisionTreeClassifier(hemophilia[,c(1,2)], hemophilia[,3])

x = hemophilia[,c(1,2)]
y = hemophilia[,3]
res = rpart(gr~., data=hemophilia, minsplit=10, minbucket=3)
plot(res)
text(res, use.n=TRUE)

}
pred = ifelse(predict(res, hemophilia)[,1] > 0.5, "career", "normal")
(tab <- table(hemophilia$gr, pred))
hemophilia
col = ifelse(predict(res, hemophilia)[,1] > 0.5, 2, 1)
par(xpd=FALSE)
plot(hemophilia[,1], hemophilia[,2], col=col, xlab="AHFactivity", ylab="AHFantigen", main="血友病の抗原・活動度による分類予測")
abline(h=0.0793)
abline(h=-0.3042)
abline(v=-0.2085)
abline(v=-0.01915)
labs=c("career", "normal")
legend("topleft",legend=labs, col=c(2,1), pch=c(1,1))

