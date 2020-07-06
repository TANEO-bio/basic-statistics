kameosu <- read.csv("KameOsu.csv")
plot(kameosu)

average <- c(mean(kameosu[,1]), mean(kameosu[,2]), mean(kameosu[,3]))
kameosu2 <- rbind(kameosu, average)
kameosu2
col <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2)
pch <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,19)
plot(kameosu2, col=col, pch=pch)
KameCor <- cor(kameosu)
PCA <- eigen(KameCor)$vectors
PCA[,1]

PC1 <- scale(kameosu) %*% PCA[,1]
PC2 <- scale(kameosu) %*% PCA[,2]
PC3 <- scale(kameosu) %*% PCA[,3]
pcscore <- cbind(PC1, PC2, PC3)
dimnames(pcscore)[[2]] <- c("第 1 主成分","第 2 主成分","第 3 主成分")
pairs(pcscore,panel=function(x,y){
  points(x,y,pch="",xlab="",ylab="")
  text(x,y,1:24)
})

#part2
olympic <- read.table("decathlon2004.txt")[,-(11:12)]
olympca <- prcomp(olympic, scale=TRUE)
var(scale(olympic) %*% olympca$rotation[,1])

rank(abs(olympca$rotation[,2]))
summary(olympca)
screeplot(olympca, main="Screeplot for Variance (with scaled data)")