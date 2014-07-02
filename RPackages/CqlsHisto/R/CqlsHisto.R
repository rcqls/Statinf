Verif.EAP<-function(Y,Loi,m=get(".m.EAP",.randomEnv),...) {
  if(missing(Loi)) Loi<-Y
  Hist.EAP(sim(Y,m),...)
  if(inherits(Loi,"distrib") && !inherits(Loi,"randExpr")) plot(Loi,add=TRUE,lwd=3) 
}

############################
# Quelques représentations #
############################
HistCurve<-function(x,breaks,add=F,...)
{
  if(missing(breaks))  H<-hist(x,plot=F,...)
  else H<-hist(x,plot=F,breaks=breaks,...)
  X<-rep(H$breaks,rep(2,length(H$breaks)))
  freq <- H$density
  Y<-c(0,rep(freq,rep(2,length(freq))),0)
  if(add) lines(X,Y,...)
  else plot(X,Y,type="l",xlab="",ylab="",...)
}

Hist <- HistCont <- function(x,add=FALSE,rect=FALSE,...) {
  if(rect) hist(x,add=add,prob=TRUE,...)
  else HistCurve(x,add=add,...)
}

HistDisc<-function(x,add=F,rect=F,...)
{
  data <- sort(unique(x))
  pas<-diff(data)
  br <- c(data[1]-pas[1]/2,data+c(pas,pas[length(pas)])/2)
  if(rect) hist(x,add=add,breaks=br,prob=T,...)
  else HistCurve(x,add=add,breaks=br,...)
}

## tool to convert par("usr") in {xlim,ylim} !!!
xylim.convert<- function(digit=7) {
  par("usr")->usr
  a <- solve(rbind(c(26,-1),c(-1,26))/25)
  list(xlim=round(as.vector(a%*%usr[1:2]),digit),ylim=round(as.vector(a%*%usr[3:4]),digit))
}


plot.EAP <- function(Y,yy,m=100,br,col,discrete=NULL,between=NULL) { #between=c(2,4) means that selection occurs when yy between 2 and 4!!! 
  
  rang <- Quantile(Y,c(0,1))
  from <- if(rang[1]==-Inf) Quantile(Y,.tailPlotDistrib) 
  else rang[1]
  to <- if(rang[2]==Inf) Quantile(Y,1-.tailPlotDistrib) 
  else rang[2]
 
 if(is.null(discrete) && inherits(Y,"random")) discrete<-is.discrete(Y)   
  
  if(missing(yy)) yy <- sim(Y,m)
  else m <- length(yy)
  if(!is.null(between)) selected <- (yy>=between[1] & yy<=between[2])
  else selected <- T
  hh <- sapply(yy,function(e) runif(1,0,ff(Y,e)))
  oldpar <- par(mfrow=c(2,1))
  plot(Y,from=from,to=to)
  if(!is.null(between)) abline(v=between)
  if(missing(col)) col <- cm.colors(m)
  else if(length(col)<m) {
    lcol <- length(col)
    col <- rep(col,lcol*(m%/%lcol)+1)[1:m]
  }
  if(!is.null(between)) col[!selected] <- 0
  points(yy,hh,pch=21,bg=col)
  if(missing(br)) br <- nclass.FD(yy)
  plot(Y,from=from,to=to)
  Hist.EAP(yy,discrete=discrete,br=br,col=col,add=T,when=selected)
  if(!is.null(between)) abline(v=between)
  par(oldpar)
}

Hist.EAP <- function(x,breaks,discrete=NULL,level.discrete=10,add=F,xlim,ylim,col,rect=T,lty=par("lty"),lwd=par("lwd"),when=T,...) { 
  centers <- sort(unique(x))
  if(is.null(discrete)) discrete <- (length(x)/length(centers)) > level.discrete
  ##cat("discrete=",discrete,"\n")
  if(discrete) {
    ## First !!! equispaced data is assumed when discrete!!!
    pas<-min(unique(diff(centers)))
    ##if(length(pas)>1) stop("equispaced data is assumed when discrete!!!")
    pas2 <- pas/2.0
    ##tab <- table(x)
    ##print(tab)
    rang <- range(x)
    centers <- seq(rang[1],rang[2],pas)
    br <- c(centers[1]-pas2,centers+pas2)
    eff <- as.vector(hist(x,br=br,plot=F)$counts)
    ##eff <- rep(0,length(centers))
    ##names(eff) <- centers
    ##eff[match(as.numeric(names(tab)),centers)] <- tab
    ##print(eff)
  } else {
    if(missing(breaks)) breaks <- nclass.FD(x)
    rang <- range(x)
    if(length(breaks)==1) {
      pas <- diff(rang)/breaks
      pas2 <- pas/2.0
    } else if(length(breaks)==2) {
      center<-breaks[1]
      pas<-breaks[2]
      pas2 <- pas/2.0
      rang <- center+c(-floor((center-rang[1])/pas+1)*pas,floor((rang[2]-center)/pas+1)*pas)
      breaks <- seq(rang[1],rang[2],pas)
    } else {
      stop("breaks have to be a number since equispaced partition is assumed")
    }
    centers <- seq(rang[1]+pas2,rang[2],pas)
    cuts <- cut(x,br=breaks)
    eff <- as.vector(table(cuts))
    ##histo <- hist(x,br=breaks,plot=FALSE)
    ##print(histo)
    ##pas <- unique(diff(histo$breaks))[1]
    ##print(pas)
    ##if(length(pas)>1) stop("trouble with hist!!!")
    ##pas2 <- pas/2
    ##eff <- histo$counts
    ##centers <- histo$mids
  }
  params <- names(list(...))
  par("usr")->usr
  if(!add && missing(xlim)) xlim <- range(centers)
  tot <- sum(eff)
  high <- 1/tot/pas
  high2 <- high/2.0
  if(!add && missing(ylim)) ylim <- c(0,high*max(eff)) 
  if(!add) {
    plot(0,0,type="n",xlim=xlim,ylim=ylim,...)
  }
  x.bricks <- rep(centers,eff)
  ## les hauteurs des briques !!!
  y.bricks <- unlist(sapply(eff[eff>0],function(i) seq(1,i)))*high
  if(missing(col)) col <- cm.colors(tot)
  else if(length(col)<tot) {
    lcol <- length(col)
    col <- rep(col,lcol*(tot%/%lcol)+1)[1:tot]
  }
  ## les briques
  if(rect && any(when)) {
    ## trop sioux!!! sort(x)[order(order(x))]==x
    if(discrete) cuts <- x
    order(order(cuts))->init.order
    bricks.x <- x.bricks[init.order]
    bricks.y <- y.bricks[init.order]
    rect(bricks.x[when]-pas2,bricks.y[when]-high,bricks.x[when]+pas2,bricks.y[when],col=col[when])
  }
  ## le contour
  freq <- eff*high
  br.contour <- c(centers[1]-pas2,centers+pas2)
  x.contour <- rep(br.contour,rep(2,length(br.contour)))
  y.contour <- c(0,rep(freq,rep(2,length(freq))),0)
  lines(x.contour,y.contour,lwd=lwd,lty=lty)
  abline(h=0)
  if(add)
     assign(".histo.EAP",list(x=x,pas=pas,x.bricks=x.bricks,y.bricks=y.bricks,centers=centers,counts=eff,col=col,discrete=discrete,x.contour=x.contour,y.contour=y.contour),envir=.randomEnv)
  else
    assign(".histo.EAP",list(x=x,pas=pas,x.bricks=x.bricks,y.bricks=y.bricks,centers=centers,counts=eff,col=col,discrete=discrete,xlim=xlim,ylim=ylim,x.contour=x.contour,y.contour=y.contour),envir=.randomEnv)
}

#HistDiscrete<-function(x,add=F,rect=F,...)
#{
#  pas<-min(diff(sort(unique(x))))
#  
#  br<-seq(min(x)-pas/2,max(x)+pas/2,pas)
#  if(rect) hist(x,add=add,breaks=br,prob=T,...)
#  else HistCurve(x,add=add,breaks=br,...)
#}

## To improve !!!
## Notice that this histo is equispaced !!! (look above to generalize)
HistDiscreteTh<-function(mods,prob,pas=NULL,fill=F,add=F,...)
{
  if(is.null(pas)) {
    if(length(mods)==1) {
      cat("Saisir le pas !!!\n")
      return(invisible())
    }
    pas<-min(diff(sort(unique(mods))))
  }
  br<-seq(min(mods)-pas/2,max(mods)+pas/2,pas)
  X<-rep(br,rep(2,length(br)))
  Densite<-prob/pas
  Y<-c(0,rep(Densite,rep(2,length(Densite))),0)
  if(!fill) {
    if(add) lines(X,Y,...)
    else plot(X,Y,type="l",...)
  } else {
    X<-c(X,X[1]);
    Y<-c(Y,0);
    if(!add) plot(X,Y,type="n",...)
    polygon(X,Y,...)
    rect(br[-length(br)],rep(0,length(mods)),br[-1],Densite)
  }
}
  
CumulDiscreteTh<-function(mods,probcum,add=F,lty=1,lwd=1,...)
{
  l<-length(mods)
  xi<-mods[1]-(mods[2]-mods[1])
  yi<-0
  xs<-mods[l]+(mods[l]-mods[l-1])
  x0<-c(xi,mods)
  y<-c(yi,probcum)
  x1<-c(mods,xs)
  if(!add) plot(c(x0,xs),c(y,y[l+1]),type="n",...)
  segments(x0,y,x1,y,lty=lty,lwd=lwd)
}

QuantileDiscreteTh<-function(mods,probcum,add=F,lty=1,lwd=1,...)
{
  l<-length(mods)
  xi<-0
  x0<-c(xi,probcum[-l])
  y<-mods
  x1<-probcum
  if(!add) plot(c(0,probcum),c(y[1],y),type="n",...)
  segments(x0,y,x1,y,lty=lty,lwd=lwd)
}
#####################
# fonction escalier #
#####################
StepHistDiscrete<-function(mods,prob)
{
  pas<-min(diff(sort(unique(mods))))
  br<-seq(min(mods)-pas/2,max(mods)+pas/2,pas)
  mi<-min(br);ma<-max(br)
  f<-function(x) {
    res<-rep(0,length(x))
    for(i in 1:length(mods)) {
      res[(br[i]<x)&&(x<=br[i+1])]<-prob[i]
    }
    res
  }
  return(f)
}
 
