mammals <- read.csv("sleep_ex2.csv")
cor(mammals)
cor(mammals, use="complete.obs")
cor(mammals, use="pairwise.complete.obs")

nomam <- na.omit(mammals)

pairs(nomam)
cor(nomam)
cor(nomam, method="s")
abs(cor(nomam) - cor(nomam, method="s"))

jnomam <- matrix(c(rank(nomam$msleep), rank(nomam$body), rank(nomam$brain), rank(nomam$life), rank(nomam$gestation)), ncol=5)
pairs(jnomam)

pairs(iris[,1:4])
cor(iris[,1:4],method="pearson")
cor(iris[,1:4],method="spearman")
