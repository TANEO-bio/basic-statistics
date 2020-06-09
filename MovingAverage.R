source("StatSci07_source.txt")
hyakka
plot(hyakka)
hyakkaT <- filter(hyakka, c(1,1,1,1,1,1,1,1,1,1,1,1))/12
ts.plot(hyakka, hyakkaT, gpars=list(lwd=c(1:3), col=c(1,2)), xlab="時間", ylab="売上高")

season<- c(1:12)
for (i in 1:12) {
  season[i] <- mean(na.omit((hyakka/hyakkaT)[seq(i,length(hyakka), by=12)]))
}
season
summary(season)
hyakka

hyakka_adj = hyakka
for (i in 1:length(hyakka)) {
  if (i%%12!=0) {hyakka_adj[i] <- hyakka[i] / season[i%%12]}
  if (i%%12==0) {hyakka_adj[i] <- hyakka[i] / season[12]}
}
ts.plot(hyakka_adj,xlab="時間", ylab="季節調整済売上高")
abline(v=1988:1999,lty=2,lwd=0.5)

dogetsuhi <- hyakka
for (i in 1:length(hyakka)) {
  if (i<13) {dogetsuhi[i] <- NA}
  if (i>=13) {dogetsuhi[i] <- 100* (hyakka[i] / hyakka[i-12] - 1)}
}
ts.plot(dogetsuhi, xlab="時間", ylab="前年同月比")
abline(v=1988:1999,lty=2,lwd=0.5)
hyakkaT
