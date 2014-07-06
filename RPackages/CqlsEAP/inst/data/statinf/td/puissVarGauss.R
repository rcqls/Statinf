puissVarGauss<-function(v0,n,bmax=50,side=">",alpha=0.05){
v<-seq(0.01,bmax,l=200)
if (side==">") {
	q<-qchisq(1-alpha,n-1)
	tmp<-1-pchisq(v0/v*q,n-1)
}
else {
	q<-qchisq(alpha,n-1)
	tmp<-pchisq(v0/v*q,n-1)
}
plot(v,tmp,type="l",ylim=c(0,1),xlim=c(-2,bmax))
abline(h=c(0,1),v=v0)
abline(h=alpha,lty=2)
}