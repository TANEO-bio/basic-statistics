heightsm <- read.csv("heightsm.csv")
head(heightsm)
dim(heightsm)
summary(heightsm)

dim(heightsm)[1]
sqrt(var(heightsm) * (dim(heightsm)[1] - 1) / dim(heightsm)[1])

MH <- heightsm[1]$Mheight
DH <- heightsm[2]$Dheight
plot(heightsm)

b1 <- var(heightsm)[2] / var(MH)
b0 <- mean(MH) - b1 * mean(DH)

plot(MH, DH, xlab="母親の身長(cm)", ylab="娘の身長(cm)")
points(mean(MH), mean(DH), cex=1.5, pch=16, col=3)
abline(b0, b1,col=3, lwd=3)
abline(a=0, b=1, col=2, lwd=3, lty=3)

mammals <- read.csv("sleep_ex4.csv")
options(width=100)
mammals[1:20,]

cor(na.omit(mammals)[-1])
pairs(mammals)
mammals2 <- mammals[,c(1,2,3,9)]
mammals2 <- na.omit(mammals2)
mammals2

length(mammals2$body **2)
length(mammals$body**1)
gbody <- log(mammals2$body)
cor(gbody, mammals2$msleep)
plot(gbody, mammals2$msleep)

X <- cbind(rep(1, length(mammals2$body)), gbody, mammals2$danger)
XTX <- t(X) %*% X
XTY <- t(X) %*% mammals2$msleep
beta <- (solve(XTX, XTY))
fittedy <- X %*% beta
sresid  <- mammals2$msleep - fittedy
plot(fittedy, sresid)

mammals2[which(fittedy == max(fittedy)),]
mammals2[which(abs(sresid) == max(abs(sresid))),]
log(0.01)
log(1.41)
max(fittedy)
mammals2$msleep[which(fittedy == max(fittedy))] - fittedy[which(fittedy == max(fittedy)),]

mammals3 <- mammals2[c(-1, -5),]
mammals3
plot(mammals2$msleep, mammals2$body)
b1 <- var(mammals3[,c(2,3)])[2] / var(mammals3$msleep)
b0 <- mean(mammals3$body) - b1 * mean(mammals3$msleep)
abline(b0, b1,col=3, lwd=3)
b0
b1
