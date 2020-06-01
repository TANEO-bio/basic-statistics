forbes <- read.table("forbes.txt")
bp <- forbes$bp
pres <- forbes$pres
lpres <- forbes$lpres
X <- cbind(rep(1, length(bp)), bp)
y <- lpres
y_hat <- X %*% beta
beta <- solve(t(X) %*% X) %*% t(X) %*% y
plot(lpres ~ bp, data = forbes)
sum((y_hat - mean(y_hat))**2) / sum((y - mean(y))**2)
e_hat <- y - y_hat
sigma2 <- sum(e_hat**2) / (length(e_hat) - 2)
sigma_beta2 <- sigma2 * solve(t(X) %*% X)
beta
sqrt(c(sigma_beta2[1], sigma_beta2[4]))
sqrt(sigma2)
sum((y_hat - mean(y_hat))**2) / sum((y - mean(y))**2)


par(mfrow = c(1, 2))
model1 <- lm(lpres ~ bp, data = forbes)
plot(lpres ~ bp, data = forbes, xlab = "Boiling point", ylab = "Log pressure", pch = 19)
abline(model1)
resid1 <- residuals(model1)
plot(resid1 ~ bp, data = forbes, xlab = "Boiling point", ylab = "Residuals", pch = 19)
abline(h = 0)


forbes <- read.table("forbes.txt")
bp <- forbes$bp
pres <- forbes$pres
lpres <- forbes$lpres
X <- cbind(rep(1, length(bp)), bp)
y <- pres
beta <- solve(t(X) %*% X) %*% t(X) %*% y
y_hat <- X %*% beta
e_hat <- y - y_hat
sigma2 <- sum(e_hat**2) / (length(e_hat) - 2)
sigma_beta2 <- sigma2 * solve(t(X) %*% X)
c(beta[1], beta[2])
sqrt(c(sigma_beta2[1], sigma_beta2[4]))
sqrt(sigma2)
sum((y_hat - mean(y_hat))**2) / sum((y - mean(y))**2)

forbes <- forbes[c(-12),]
bp <- forbes$bp
pres <- forbes$pres
lpres <- forbes$lpres
X <- cbind(rep(1, length(bp)), bp)
y <- pres
beta <- solve(t(X) %*% X) %*% t(X) %*% y
y_hat <- X %*% beta
e_hat <- y - y_hat
sigma2 <- sum(e_hat**2) / (length(e_hat) - 2)
sigma_beta2 <- sigma2 * solve(t(X) %*% X)
c(beta[1], beta[2])
sqrt(c(sigma_beta2[1], sigma_beta2[4]))
sqrt(sigma2)
sum((y_hat - mean(y_hat))**2) / sum((y - mean(y))**2)


forbes <- read.table("forbes.txt")
bp <- forbes$bp
pres <- forbes$pres
lpres <- forbes$lpres
X <- cbind(rep(1, length(bp)), bp)
y <- lpres
beta <- solve(t(X) %*% X) %*% t(X) %*% y
y_hat <- X %*% beta
e_hat <- y - y_hat
sigma2 <- sum(e_hat**2) / (length(e_hat) - 2)
sigma_beta2 <- sigma2 * solve(t(X) %*% X)
c(beta[1], beta[2])
sqrt(c(sigma_beta2[1], sigma_beta2[4]))
sqrt(sigma2)
sum((y_hat - mean(y_hat))**2) / sum((y - mean(y))**2)

forbes <- forbes[c(-12),]
bp <- forbes$bp
pres <- forbes$pres
lpres <- forbes$lpres
X <- cbind(rep(1, length(bp)), bp)
y <- lpres
beta <- solve(t(X) %*% X) %*% t(X) %*% y
y_hat <- X %*% beta
e_hat <- y - y_hat
sigma2 <- sum(e_hat**2) / (length(e_hat) - 2)
sigma_beta2 <- sigma2 * solve(t(X) %*% X)
c(beta[1], beta[2])
sqrt(c(sigma_beta2[1], sigma_beta2[4]))
sqrt(sigma2)
sum((y_hat - mean(y_hat))**2) / sum((y - mean(y))**2)

forbes <- read.table("forbes.txt")
forbes <- forbes[c(-12),]
bp <- forbes$bp
pres <- forbes$pres
lpres <- forbes$lpres
X <- cbind(rep(1, length(bp)), bp)
y <- lpres
beta <- solve(t(X) %*% X) %*% t(X) %*% y
beta1 <- beta[2]
y_hat <- X %*% beta
e_hat <- y - y_hat
sigma2 <- sum(e_hat**2) / (length(e_hat) - 2)
sigma_beta2 <- sigma2 * solve(t(X) %*% X)
df <- length(y) - 2
tdf <- qt(1 - 0.025, df)
beta1_25 <- beta1 - tdf * sqrt(sigma_beta2[4])
beta1_975 <- beta1 + tdf * sqrt(sigma_beta2[4])
beta1_25
beta1_975

forbes <- read.table("forbes.txt")
bp <- forbes$bp
pres <- forbes$pres
lpres <- forbes$lpres
u1 = 1/((5/9)*bp+255.37)
X  <- cbind(rep(1, length(y)), scale(u1))
y <- lpres
y<- scale(y)
beta <- solve(t(X) %*% X) %*% t(X) %*% y
y_hat3 <- X %*% beta
beta

X <- cbind(rep(1, length(u1)), u1)
y <- lpres
beta <- solve(t(X) %*% X) %*% t(X) %*% y
y_hat2 <- X %*% beta
plot(y_hat2, y_hat3, xlab="model2によるlpresの予測値", ylab = "model3によるlpresの予測値")
e2 <- y - y_hat2
e3 <- y - y_hat3
sqrt(sum(e2**2) / (length(e2) - 2))
sqrt(sum(e3**2) / (length(e3) - 2))
sum((y_hat3 - mean(y_hat3))**2) / sum((y - mean(y))**2)


