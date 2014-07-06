if(1) {
require(CqlsRandom)

## Init
N<-2000000
Y<-ChooseIn(0:3,c(N-(N1+N2+N3),N1,N2,N3))
n<-1000
YY<-Sample(Y,n)
m<-10000
## Expérience aléatoire
##prodA
N1<-200000
N2<-N3<-0
yy.sim<-NULL
tmp<-sim(mean(YY),m)
yy.sim<-cbind(yy.sim,sort(tmp))
N1<-300000
tmp<-sim(mean(YY),m)
yy.sim<-cbind(yy.sim,sort(tmp))
N1<-400000
tmp<-sim(mean(YY),m)
yy.sim<-cbind(yy.sim,sort(tmp))
##prodB
N1<-100000
N2<-N3<-20000
tmp<-t(sapply(sim(c(mean(YY),sd(YY)),m),function(x) x))
print(order(tmp[,1]))
yy.sim<-cbind(yy.sim,tmp[order(tmp[,1]),])
N1<-200000
tmp<-t(sapply(sim(c(mean(YY),sd(YY)),m),function(x) x))
yy.sim<-cbind(yy.sim,tmp[order(tmp[,1]),])
N1<-300000
tmp<-t(sapply(sim(c(mean(YY),sd(YY)),m),function(x) x))
yy.sim<-cbind(yy.sim,tmp[order(tmp[,1]),])
##addendum : manquer les deltaEst
muEst.sim<-yy.sim[,c(1:3,4,6,8)]
muEst.sim<-cbind(muEst.sim,sort((yy.sim[,4]-.15)/yy.sim[,5]*sqrt(1000))
muEst.sim<-cbind(muEst.sim,sort((yy.sim[,6]-.15)/yy.sim[,7]*sqrt(1000))
muEst.sim<-cbind(muEst.sim,sort((yy.sim[,8]-.15)/yy.sim[,9]*sqrt(1000))

save(yy.sim,file="simEch.RData")
}
