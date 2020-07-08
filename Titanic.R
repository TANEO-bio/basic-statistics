source("titanic_UTF.txt")

# Visualization
tab <- table(survive=titanic$survived, sibsp=titanic$pclass)
prop.table(tab,2)

tab <- table(survive=titanic$survived, sibsp=titanic$sex)
prop.table(tab,2)

breaks.all <- hist(titanic$age)$breaks
par(mfrow=c(2,1))
hist(titanic$age[titanic$survived==1],breaks=breaks.all,main="生存者の年齢分布", xlab="年齢")
hist(titanic$age[titanic$survived==0],breaks=breaks.all,main="死亡者の年齢分布", xlab="年齢")

tab <- table(survive=titanic$survived, sibsp=titanic$sibsp)
prop.table(tab,2)
tab

tab <- table(survive=titanic$survived, parch=titanic$parch)
prop.table(tab,2)
tab

logfare <- log(titanic$fare)
breaks.all <- hist(logfare)$breaks
par(mfrow=c(2,1))
hist(logfare[titanic$survived==1],breaks=breaks.all,main="生存者の乗船料金の分布(対数変換済)", xlab="log(乗船料金)")
hist(logfare[titanic$survived==0],breaks=breaks.all,main="死亡者の乗船料金の分布(対数変換済)", xlab="log(乗船料金)")

tab <- table(survive=titanic$survived, embarked=titanic$embarked)
prop.table(tab,2)
tab

# Prediction (DecisionTree)
tree.d <- rpart(survived~., data=titanic[,-1])
par(xpd=TRUE)
plot(tree.d)
text(tree.d, use.n=TRUE)

# Plunning
tree.train <- rpart(survived~., data=na.omit(titanic.train[,-1]), cp=0)
tree3 <- prune(tree.train, cp=0.003)
tree5 <- prune(tree.train, cp=0.005)
tree7 <- prune(tree.train, cp=0.0075)
tree10 <- prune(tree.train, cp=0.01)

par(xpd=TRUE)
plot(tree3, main="cp=0.003")
text(tree3, use.n=TRUE)

par(xpd=TRUE)
plot(tree5, main="cp=0.005")
text(tree5, use.n=TRUE)

par(xpd=TRUE)
plot(tree7, main="cp=0.0075")
text(tree7, use.n=TRUE)

par(xpd=TRUE)
plot(tree10, main="cp=0.01")
text(tree10, use.n=TRUE)

pred <- ifelse(predict(tree3, titanic.test[,-1])[,1] >= 0.5, 0, 1)
test <- titanic.test[,2]
(tab <- table(pred, test))
1-sum(diag(tab)) / sum(tab)

pred <- ifelse(predict(tree5, titanic.test[,-1])[,1] >= 0.5, 0, 1)
test <- titanic.test[,2]
(tab <- table(pred, test))
1-sum(diag(tab)) / sum(tab)

pred <- ifelse(predict(tree7, titanic.test[,-1])[,1] >= 0.5, 0, 1)
test <- titanic.test[,2]
(tab <- table(pred, test))
1-sum(diag(tab)) / sum(tab)

pred1 <- ifelse(predict(tree10, titanic.test[,-1])[,1] >= 0.5, 0, 1)
test <- titanic.test[,2]
(tab <- table(test, pred))
1-sum(diag(tab)) / sum(tab)

# LDA vs DecisionTree
X1 <- titanic.train[titanic.train$survived==1,5:8]
X2 <- titanic.train[titanic.train$survived==0,5:8]
X2 <- na.omit(X2)
X1 <- as.matrix(X1)
X2 <- as.matrix(X2)

x <- titanic.test[,5:8]
N1 <- dim(X1)[1]
N2 <- dim(X2)[1]
mu1 <- apply(X1,2,sum)/N1
mu2 <- apply(X2,2,sum)/N2
ab <- (mu1 + mu2)*0.5
gh <- mu1 - mu2

sigma1 <- var(X1) * (dim(X1)[1]-1)/dim(X1)[1]
sigma2 <- var(X2) * (dim(X2)[1]-1)/dim(X2)[1]
omega <- solve((sigma1 + sigma2) / (dim(X1)[1] + dim(X2)[1] - 2))

fisher_LDA <- function(x) {(omega[1,] %*% gh) * x[1] + (omega[2,] %*% gh) * x[2] + (omega[3,] %*% gh) * x[3] + (omega[4,] %*% gh) * x[4] - gh[1]*(ab%*%omega[,1]) - gh[2]*(ab%*%omega[,2]) - gh[3]*(ab%*%omega[,3]) - gh[4]*(ab%*%omega[,4])}
pred <- as.vector(ifelse(apply(x,1,fisher_LDA)<0,0,1))
true <- titanic.test$survived
(tab <- table(true, pred))

bayesianLDA <- function(x) {(omega[1,] %*% gh) * x[1] + (omega[2,] %*% gh) * x[2] + (omega[3,] %*% gh) * x[3] + (omega[4,] %*% gh) * x[4] - gh[1]*(ab%*%omega[,1]) - gh[2]*(ab%*%omega[,2]) - gh[3]*(ab%*%omega[,3]) - gh[4]*(ab%*%omega[,4])} + log(N1/N2)
pred_b <- as.vector(ifelse(apply(x,1,bayesianLDA)<0,0,1))
true <- titanic.test$survived
(tab <- table(true, pred_b))
