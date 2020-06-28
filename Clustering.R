mammals <- read.csv("sleep_ex2.csv")
mammals2<- na.omit(mammals)
mammals3<- scale(mammals2)
mammals3.d <- dist(mammals3)
mammals3.hc <- hclust(mammals3.d, method="average")
summary(mammals3.hc)
mammals3.hc$merge
plot(mammals3.hc)

mammals3_single.hc <- hclust(mammals3.d, method="single")
mammals3_complete.hc<- hclust(mammals3.d, method="complete")
plot(mammals3_single.hc, hang=-1)
plot(mammals3_complete.hc, hang=-1)

cor(dist(mammals3), cophenetic(mammals3.hc))
cor(dist(mammals3), cophenetic(mammals3_single.hc))
cor(dist(mammals3), cophenetic(mammals3_complete.hc))
