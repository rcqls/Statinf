
## class EAP.mean
EAP.mean <- function(YY,m,col=cm.colors(m)) {
  obj <- list(YY=YY,m=m,col=col)
  class(obj) <- "EAP.mean"
  sim(obj)
  ## mean distribution
  update(obj)
  obj
}

print.EAP.mean <- function(obj) {
  cat("EAP (m=",obj$m,") for mean with ",sep="")
  WhatIs(obj$YY)
}

length.EAP.mean <- function(obj) obj$m

update.EAP.mean <- function(obj,col) {
  objname <- as.character(substitute(obj))
  m <- obj$m
  n <- length(obj$YY)
  ## color
  if(missing(col)) col <- cm.colors(m)
  else if(length(col)<m) {
    lcol <- length(col)
    col <- rep(col,lcol*(m%/%lcol)+1)[1:m]
  }
  obj$col <- col
  ## mean distribution
  obj$EE.mean <- EE(obj$YY$rv)$value
  obj$VV.mean <- (VV(obj$YY$rv)$value)/n
  ## var distribution
  obj$EE.seMean <- VV(obj$YY$rv)$value/n
  obj$VV.seMean <- (MM.EAP(obj$YY$rv,4)-obj$EE.seMean^2)/n^3
  ## hh
  dist.mean <- Norm(obj$EE.mean,sqrt(obj$VV.mean))
  obj$hh.mean <- sapply(mean(obj),function(e) runif(1,0,ff(dist.mean,e)))
  dist.seMean <- Norm(obj$EE.seMean,sqrt(obj$VV.seMean))
  obj$hh.seMean <- sapply(var(obj)/n,function(e) runif(1,0,ff(dist.seMean,e)))
  obj$hh.delta <- sapply((mean(obj)-obj$EE.mean)/sqrt(var(obj)/n),function(e) runif(1,0,ff(Norm(),e)))
  assign(objname,obj,parent.frame())
}

Random.EAP.mean <- function(obj) {
   objname <- as.character(substitute(obj))
   obj$yy.sim <- sim(obj$YY,obj$m)
   update(obj)
   assign(objname,obj,parent.frame())
}

"[[.EAP.mean" <- function(obj,i) {
  obj$yy.sim[[i]]
}

length.EAP.mean <- function(obj) {
  obj$m
}

mean.EAP.mean <- function(obj) {
  mean(obj$yy.sim)
}

var.EAP.mean <- function(obj) {
  var(obj$yy.sim)
}

sd.EAP.mean <- function(obj) {
  sd(obj$yy.sim)
}


plot.EAP.mean <- function(obj,type="all",alpha=1,selected) { ##type="mean", "var", "delta","seMean" and "all"
  if(type=="all") {
    Plot.EAP(obj,alpha=alpha)
    return(invisible())
  }
 .Par.EAP<-.randomEnv$.Par.EAP #get(".Par.EAP",.randomEnv) 
 print(.Par.EAP)
  n <- length(obj$YY)
  if(missing(selected)) selected <- FALSE
  if(is.numeric(selected)) selected <- (1:obj$m) %in% selected

  if(alpha!=1) {
    q <- qnorm(1-alpha/2)
    m <- mean(obj)
    se <- sqrt(var(obj)/length(obj$YY))
  }
  
  switch(type,
         delta={
           plot(Norm(),lwd=3,col="blue",xlab="delta",ylab="")
           abline(h=0)
           abline(v=0,lwd=3,col="purple")
           delta.est <- (mean(obj)-obj$EE.mean)/sqrt(var(obj)/n)
            if(alpha!=1) {
             abline(v=c(-q,q),lwd=3)
             area(Norm(),prob=c(.randomEnv$.tailPlotDistrib,alpha/2),col="green")
             area(Norm(),prob=1-c(alpha/2,.randomEnv$.tailPlotDistrib),col="green")
           }
           points(delta.est[!selected],obj$hh.delta[!selected],bg=obj$col[!selected],pch=21,cex=.Par.EAP$cex)
           if(any(selected)) {
             abline(v=delta.est[selected])
             cat("Delta Est. =");print(delta.est[selected])
             points(delta.est[selected],obj$hh.delta[selected],bg=obj$col[selected],pch=21,cex=.Par.EAP$sel.cex)
           }
         },
         seMean={
           plot(Norm(obj$EE.seMean,sqrt(obj$VV.seMean)),lwd=3,col="red",xlab="standard error",ylab="")
           abline(h=0)
           points(var(obj)[!selected]/n,obj$hh.seMean[!selected],bg=obj$col[!selected],pch=21,cex=.Par.EAP$cex)
           if(any(selected)) {
             abline(v=var(obj)[selected]/n)
             cat("Standard Error =");print(var(obj)[selected]/n)
             points(var(obj)[selected]/n,obj$hh.seMean[selected],bg=obj$col[selected],pch=21,cex=.Par.EAP$sel.cex)
           }
         },
         var={
           plot(Norm(obj$EE.seMean*n,sqrt(obj$VV.seMean)*n),lwd=3,col="red",xlab=paste("var of",WhatIs(obj$YY$rv,aff=FALSE)),ylab="")
           points(var(obj)[!selected],obj$hh.seMean[!selected]/n,bg=obj$col[!selected],pch=21,cex=.Par.EAP$cex)
           if(any(selected)) {
             abline(v=var(obj)[selected])
             points(var(obj)[selected],obj$hh.seMean[selected]/n,bg=obj$col[selected],pch=21,cex=.Par.EAP$sel.cex)
           }
         },
         mean={
           plot(Norm(obj$EE.mean,sqrt(obj$VV.mean)),lwd=3,col="red",xlab="mean",ylab="")
           abline(h=0)
           abline(v=obj$EE.mean,lwd=3,col="purple")
           if(any(selected)) {
             if(alpha!=1) {
               arrows(m[selected]+c(-q*se[selected],q*se[selected]),obj$hh.mean[selected],m[selected]+c(q*se[selected],-q*se[selected]),obj$hh.mean[selected],angle=90,lwd=3,col=obj$col[selected])
               arrows(m[selected]+c(-q*se[selected],q*se[selected]),obj$hh.mean[selected],m[selected]+c(q*se[selected],-q*se[selected]),obj$hh.mean[selected],angle=90)
             }
             points(mean(obj)[!selected],obj$hh.mean[!selected],bg=obj$col[!selected],pch=21,cex=.Par.EAP$cex)
             abline(v=mean(obj)[selected])
             cat("Mean =");print(mean(obj)[selected])
             points(mean(obj)[selected],obj$hh.mean[selected],bg=obj$col[selected],pch=21,cex=.Par.EAP$sel.cex)
             
             
           } else {
             good <- TRUE
             if(alpha!=1) {
               abline(v=obj$EE.mean,lwd=3,col="black")
               q <- qnorm(1-alpha/2)
               m <- mean(obj)
               se <- sqrt(var(obj)/length(obj$YY))
               good <-abs(m-obj$EE.mean)<q*se 
               arrows(m+c(-q*se,q*se),obj$hh.mean,m+c(q*se,-q*se),obj$hh.mean,angle=90,lwd=3,col=obj$col)
               arrows(m+c(-q*se,q*se),obj$hh.mean,m+c(q*se,-q*se),obj$hh.mean,angle=90)
             }
             if(length(good)>1) points(mean(obj)[!good],obj$hh.mean[!good],bg=obj$col[!good],pch=21,cex=.Par.EAP$sel.cex)
             points(mean(obj)[good],obj$hh.mean[good],bg=obj$col[good],pch=21,cex=.Par.EAP$cex)
           }
         })
}

Plot.EAP <- function(obj,selected=FALSE,alpha=1) {
  layout(matrix(c(1,3,2,3),2))
  plot(obj,"mean",selected=selected,alpha=alpha)
  plot(obj,"seMean",selected=selected)
  plot(obj,"delta",selected=selected,alpha=alpha)
  ##par(mfrow=c(1,1))
  ##identify((mean(yy)-yy$EE.mean)/sqrt(var(yy)/yy$YY$n),yy$hh.delta,n=1,plot=F)
}



identify.EAP.mean <- function(obj,alpha=.05) {
  n <- length(obj$YY)
  delta.est <-(mean(obj)-obj$EE.mean)/sqrt(var(obj)/n)
  Plot.EAP(obj)
  repeat {
    i <- identify(delta.est,obj$hh.delta,n=1,plot=F)
    if(length(i)==0) break
    Plot.EAP(obj,selected=i,alpha=alpha)
  }
  Plot.EAP(obj,alpha=alpha)
  par(mfrow=c(1,1))
}

## EAP.cont #######################

EAP.cont <- function(rv,m=100,col=cm.colors(m)) {
  obj <- list(rv=rv,m=m,col=col,br=8,plotType="n")
  class(obj) <- c("EAP.cont","EAP.common")
  sim(obj)
  ## centre, étendu
  rang <- range(obj$sim)
  obj$etendu <- diff(rang)
  obj$centre <- sum(rang)/2
  ## mean distribution
  update(obj)
  obj
}


update.EAP.cont <- function(obj,add=0,col,recalc=FALSE) {
  objname <- as.character(substitute(obj))
  if(add) {
    if(add<0) {
      if(obj$m+add<=.randomEnv$.Par.EAP$m.min) return(invisible())
      obj$m <- obj$m+add
      obj$sim <- obj$sim[1:obj$m]
      obj$hh <- obj$hh[1:obj$m]
    } else {
      obj$m <- obj$m+add
      newSim <- sim(obj$rv,add)
      obj$sim <- c(obj$sim,newSim)
      obj$hh <- c(obj$hh,sapply(newSim,function(e) runif(1,0,ff(obj$rv,e))))
    }
  } else {
    m <- obj$m
## distribution
    obj$EE <- EE(obj$rv)$value
    obj$VV <- VV(obj$rv)$value
## hh
    obj$hh<- sapply(obj$sim,function(e) runif(1,0,ff(obj$rv,e)))
  }
  
## color
  if(missing(col)) col <- cm.colors(obj$m)
  else if(length(col)<obj$m) {
    lcol <- length(col)
    col <- rep(col,lcol*(obj$m%/%lcol)+1)[1:obj$m]
  }
  obj$col <- col
.tailPlotDistrib<-.randomEnv$.tailPlotDistrib
## bounds
  if(is.null(obj$from) || recalc) {
    rv <- obj$rv
    rang <- Quantile(rv,c(0,1))
    from <- if(rang[1]==-Inf) Quantile(rv,.tailPlotDistrib) 
    else rang[1]
    to <- if(rang[2]==Inf) Quantile(rv,1-.tailPlotDistrib) 
    else rang[2]
    obj$from <- from
    obj$to <- to
  }
  
  assign(objname,obj,parent.frame())
}

## EAP.disc #######################

EAP.disc <- function(rv,boundsArea,pop.size=Inf,m=100,col=cm.colors(m)) {
  obj <- list(rv=rv,m=m,col=col,br=8,step=1,boundsArea=boundsArea,pop.size=pop.size,plotType="n")
  class(obj) <- c("EAP.disc","EAP.common")
  sim(obj)
## centre, étendu
  rang <- range(obj$sim)
  obj$etendu <- diff(rang)
  obj$centre <-mean(rang)
## mean distribution
  update(obj)
  obj
}


update.EAP.disc <- function(obj,add=0,col,recalc=FALSE) {
  objname <- as.character(substitute(obj))
  if(add) {
    if(add<0) {
      if(obj$m+add<=.randomEnv$.Par.EAP$m.min) return(invisible())
      obj$m <- obj$m+add
      obj$sim <- obj$sim[1:obj$m]
      obj$hh <- obj$hh[1:obj$m]
     obj$xx <-  obj$xx[1:obj$m]
} else {
      obj$m <- obj$m+add
      newSim <- sim(obj$rv,add)
      obj$sim <- c(obj$sim,newSim)
      obj$xx<-c(obj$xx, newSim+runif(add,-obj$step/2.0,obj$step/2.0)) 
      obj$hh <- c(obj$hh,sapply(newSim,function(e) runif(1,0,ff(obj$rv,e)/obj$step)))
}
} else {
    m <- obj$m
## distribution
    obj$EE <- EE(obj$rv)$value
    obj$VV <- VV(obj$rv)$value
## hh
    obj$hh<- sapply(obj$sim,function(e) runif(1,0,ff(obj$rv,e)/obj$step))
   obj$xx <- obj$sim+runif(m,-obj$step/2.0,obj$step/2.0) 
}
  
## color
  if(missing(col)) col <- cm.colors(obj$m)
  else if(length(col)<obj$m) {
    lcol <- length(col)
    col <- rep(col,lcol*(obj$m%/%lcol)+1)[1:obj$m]
}
  obj$col <- col
.tailPlotDistrib<-.randomEnv$.tailPlotDistrib
## bounds
  if(is.null(obj$from) || recalc) {
    rv <- obj$rv
    rang <- Quantile(rv,c(0,1))
    from <- if(rang[1]==-Inf) Quantile(rv,.tailPlotDistrib) 
    else rang[1]
    to <- if(rang[2]==Inf) Quantile(rv,1-.tailPlotDistrib) 
    else rang[2]
    obj$from <- from
    obj$to <- to
}
  
  assign(objname,obj,parent.frame())
}
## EAP.common methods ###################
print.EAP.common <- function(obj) {
  cat("EAP (m=",obj$m,") for r.v. ",sep="")
  WhatIs(obj$rv)
}

length.EAP.common <- function(obj) obj$m

Random.EAP.common <- function(obj) {
  objname <- as.character(substitute(obj))
   obj$sim <- sim(obj$rv,obj$m)
   update(obj)
   assign(objname,obj,parent.frame())
}



plot.EAP.cont <- function(obj,go,selected=NULL,type="between",close=TRUE) {
  ## type="between" or anything else
  ## dir="up" or "down" or nothing

  ## in order to save obj$br when go is set
  objname <- as.character(substitute(obj))
  
  rv <- obj$rv
  from <- obj$from
  to <- obj$to

 .Par.EAP<-.randomEnv$.Par.EAP 
  
  if(!missing(go)) go(obj)
  m <- obj$m

  between <- NULL
  if(type=="between") {
    if(!is.null(selected)) {
      between <- selected
      selected <- (obj$sim>=between[1] & obj$sim<=between[2])
    } else selected <- T
  }

  ###oldpar <- par(mfrow=c(2,1))

  par(bg="white")
  split.screen(c(2,1))

  xlab <- substitute(paste(y[group("[",1,"]")],",...,",y[group("[",m,"]")]," abscisses de m=",m," points choisis au hasard"),list(m=length(obj)))
  
  plot(rv,from=from,to=to,lwd=3,col="red",xlab=xlab,ylab="")
  abline(h=0)
  if(!is.null(between)) {
    area(obj$rv,between,col="green")
    abline(v=between,lwd=3)
  }
  col <- obj$col
  if(type=="between") {
    if(!is.null(between)) col[!selected] <- 0
  } else {
    col[selected] <- .Par.EAP$sel.col
    
  }
  
  points(obj$sim,obj$hh,pch=21,bg=col,cex=.Par.EAP$cex)
  if(type!="between" && length(selected)>0) {
    abline(v=obj$sim[selected],lwd=3,col=col[selected])
    abline(v=obj$sim[selected])
    points(obj$sim[selected],obj$hh[selected],pch=21,bg=col[selected],cex=.Par.EAP$sel.cex)
  }
  

  screen(2)
  xlab <- substitute(paste("histogramme des ",y[group("[",1,"]")],",...,",y[group("[",m,"]")]),list(m=length(obj)))
  plot(rv,from=from,to=to,lwd=3,col="red",xlab=xlab,ylab="",type=obj$plotType)
  abline(h=0)
  if(type!="between") {
    col[selected] <- .Par.EAP$sel.col
  }
  if(type=="between" && !is.null(between)) {
    area(obj$rv,between,col="green")
  }

  ####Hist.EAP(obj$sim,discrete=FALSE,br=obj$br,col=col,add=T)
  Hist.EAP(obj$sim,discrete=FALSE,br=c(obj$centre,obj$etendu/obj$br),col=col,add=T)##,when=selected)
  if(type=="between" && !is.null(between)) {
    abline(v=between,lwd=3)
  }
  if(type != "between") {
    abline(v=obj$sim[selected],lwd=3,col=col[selected])
    abline(v=obj$sim[selected])
  }

  ##in order to save obj$br when go is set
  assign(objname,obj,parent.frame())
  if(close) close.screen(all=TRUE)
  ##par(oldpar)
}



identify.EAP.cont <- function(obj) {
  plot(obj,close=FALSE)
  repeat {
    screen(1,FALSE)
    i <- identify(obj$sim,obj$hh,n=1,plot=F)
    close.screen(all=TRUE)
    if(length(i)==0) break
##cat("sel=",i,"\n")
    plot(obj,selected=i,type="",close=FALSE)
}
  plot(obj)
##close.screen(all=TRUE)
}


plot.EAP.disc <- function(obj,go,selected=NULL,type="between",close=TRUE) {
## type="between" or anything else
## dir="up" or "down" or nothing
## in order to save obj$br when go is set
  objname <- as.character(substitute(obj))
  
  rv <- obj$rv
  from <- obj$from
  to <- obj$to

 .Par.EAP<-.randomEnv$.Par.EAP 
  
  if(!missing(go)) go(obj)
  m <- obj$m

  between <- NULL
  if(type=="between") {
    if(!is.null(selected)) {
      between <- selected
      selected <- (obj$sim>=between[1] & obj$sim<=between[2])
} else selected <- T
}

###oldpar <- par(mfrow=c(2,1))

  par(bg="white")
  split.screen(c(2,1))

  xlab <- substitute(paste(y[group("[",1,"]")],",...,",y[group("[",m,"]")]," abscisses de m=",m," points choisis au hasard"),list(m=length(obj)))
  
  plot(rv,from=from,to=to,lwd=3,col="red",rect=TRUE)
  abline(h=0)
  if(!is.null(between)) {
    b<-obj$from +ceiling((between-obj$from)/obj$step)*obj$step
    b[2]<-b[2]-1 
    area(obj$rv,between,col="green")
    abline(v=between,lwd=3)
}
  col <- obj$col
  if(type=="between") {
    if(!is.null(between)) col[!selected] <- 0
    } else {
       col[selected] <- .Par.EAP$sel.col  
    }

   print(obj$pop.size) 
  if(obj$pop.size==Inf)
    segments(obj$sim-obj$step/2.0,obj$hh,obj$sim+obj$step/2.0,obj$hh,col=col,lwd=1) 
  else {
#supp<-seq(Quantile(obj$rv,0), Quantile(obj$rv,1),by=obj$step) ##todo SUPPORT!!!!  
    hh2 <- 1/obj$step/obj$pop.size/2.0
    rect(obj$sim-obj$step/2.0,obj$hh-hh2,obj$sim+obj$step/2.0,obj$hh+hh2,col=col) 
  }
  points(obj$xx,obj$hh,pch=21,bg=col,cex=.Par.EAP$cex)  
   
  if(type!="between" && length(selected)>0) {
    abline(v=obj$sim[selected],lwd=3,col=col[selected])
    abline(v=obj$sim[selected])
    lines(obj$sim[selected]+c(-1,1)*obj$step/2.0,rep(obj$hh[selected],2),lwd=3,col=col[selected])
    points(obj$xx[selected],obj$hh[selected],pch=21,bg=col[selected],cex=.Par.EAP$sel.cex)
}
  
  screen(2)
  xlab <- substitute(paste("histogramme des ",y[group("[",1,"]")],",...,",y[group("[",m,"]")]),list(m=length(obj)))
  plot(rv,from=from,to=to,lwd=3,col="red",rect=TRUE,type=obj$plotType)
  abline(h=0)
  if(type!="between") {
    col[selected] <- .Par.EAP$sel.col
}
  if(type=="between" && !is.null(between)) {
    area(obj$rv,between,col="green")
}

####Hist.EAP(obj$sim,discrete=FALSE,br=obj$br,col=col,add=T)
  Hist.EAP(obj$sim,discrete=TRUE,col=col,add=T)##,when=selected)
  if(type=="between" && !is.null(between)) {
    abline(v=between,lwd=3)
}
  if(type != "between") {
    abline(v=obj$sim[selected],lwd=3,col=col[selected])
    abline(v=obj$sim[selected])
}

##in order to save obj$br when go is set
  assign(objname,obj,parent.frame())
  if(close) close.screen(all=TRUE)
##par(oldpar)
}

identify.EAP.disc <- function(obj) {
  plot(obj,close=FALSE)
  repeat {
    screen(1,FALSE)
    i <- identify(obj$xx,obj$hh,n=1,plot=F)
    close.screen(all=TRUE)
    if(length(i)==0) break
##cat("sel=",i,"\n")
    plot(obj,selected=i,type="",close=FALSE)
}
  plot(obj)
##close.screen(all=TRUE)
}

up <- function(obj,...) UseMethod("up")

down <- function(obj,...) UseMethod("down")

more <- function(obj,...) UseMethod("more")

less <- function(obj,...) UseMethod("less")

play <- function(obj,...) UseMethod("play")

#nothing to do in particular in the discrete case
up.default <- function(obj) {}
down.default <- function(obj) {}

up.EAP.cont <- function(obj) {
  objname <- as.character(substitute(obj))
  obj$br <- 2*obj$br ## assumed to be an integer
  assign(objname,obj,parent.frame())
}

down.EAP.cont <- function(obj) {
  objname <- as.character(substitute(obj))
  br <- obj$br ## assumed to be an integer
  br <- br/2
  if(br>=2) obj$br <- br
   assign(objname,obj,parent.frame())
}

more.EAP.common <- function(obj,m=100) {
  objname <- as.character(substitute(obj))
  update(obj,add=m)
  assign(objname,obj,parent.frame())
}

less.EAP.common <- function(obj,m=100) {
  objname <- as.character(substitute(obj))
  update(obj,add=-m)
  assign(objname,obj,parent.frame())
}

play.EAP.common <- function(obj,bounds) {
  objname <- as.character(substitute(obj))
  m <- 100
  selMode <- "plot" # "area" or "identify"
  if(inherits(obj,"EAP.disc")) pop.size.old<-obj$pop.size 
  else pop.size.old<-Inf 
  plot(obj)
  if(missing(bounds)) {
    .histo.EAP<.randomEnv$.histo.EAP
    bricks <- unique(.histo.EAP$x.bricks)-.histo.EAP$pas/2.0
    if(inherits(obj,"EAP.cont")) bounds <- obj$centre+c(0,obj$etendu/4)
    if(inherits(obj,"EAP.disc")) bounds <- obj$boundsArea
    ##bricks[.5*length(bricks)+c(0,3)] ##QQ(obj$rv,c(.5,.75)
  }
  repeat {
    switch(menu(c("plus de réalisations","moins de réalisations","plus d'intervalles","moins d'intervalles","selection point","mode normal/surface","borne ->","borne <-","pop. finie","nouvelle simulation","turbo","repartition théorique"))+1,
           break,
           {
             more(obj,m)
             ##plot(obj)
           },
           {
             less(obj,m)
             ##plot(obj)
           },
           {
             up(obj)
             ##plot(obj)
             ##plot(obj,up)
           },
           {
             down(obj)
             ##plot(obj)
             ##plot(obj,down)
           },
           {
             selMode <- "identify"
             identify(obj)
           },
           {
             if(selMode=="area") selMode <- "normal"
             else selMode <- "area"
           }, 
           {
             bounds[2] <- bounds[2]+diff(bounds)
           },
           {
             bounds[2] <- bounds[2]-diff(bounds)/2
           },
           {
              if(obj$pop.size==Inf) obj$pop.size<-pop.size.old
              else obj$pop.size<-Inf
             print(obj$pop.size)  
           },
          {
            update(obj)
          }, 
           if(m==100) m <- 1000
           else m <- 100
           ,
           if(obj$plotType=="l") obj$plotType<-"n"
           else obj$plotType="l"
           )
    if(selMode=="identify") selMode <- "normal"
    else if(selMode=="area") {
      plot(obj,sel=bounds)
      cat("Bornes=[",bounds[1],",",bounds[2],"]\n")
      cat("\nPour m=",obj$m," -> aire=",nb <- sum(obj$sim>=bounds[1] & obj$sim<=bounds[2]),"/",obj$m,"=",nb/obj$m,"\n")
      cat("Pour m infini -> aire=",diff(FF(obj$rv,bounds)),"\n")
    }
    else plot(obj) ## selMode=="normal"
    
  }
  obj$pop.size<-pop.size.old #restore the old pop.size 
## Not so nice and leads to some bug when called with play(EAP.disc(...))
## result is then saved in EAP.disc!!!!
##assign(objname,obj,parent.frame())
}

