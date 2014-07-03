## methods
seMean<-function(obj,...) UseMethod("seMean")
seVar<-function(obj,...) UseMethod("seVar")
seDMean<-function(obj,...) UseMethod("seDMean")
seDMeanG<-function(obj,...) UseMethod("seDMeanG")
seDVar<-function(obj,...) UseMethod("seDVar")
seRMean<-function(obj,...) UseMethod("seRMean")
seRVar<-function(obj,...) UseMethod("seRVar")
coefLM<-function(obj,...) UseMethod("coefLM")
seCoefLM<-function(obj,...) UseMethod("seCoefLM")
varLM<-function(obj,...) UseMethod("varLM")
seVarLM<-function(obj,...) UseMethod("seVarLM")

## For cqlsRandom!!!
## added to Summary group !!!
seMean.dynExpr <- function(e1,...) {
  .Generic <<- "seMean"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

seDMean.dynExpr <- function(e1,...) {
  .Generic <<- "seDMean"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

seRMean.dynExpr <- function(e1,...) {
  .Generic <<- "seRMean"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

seVar.dynExpr <- function(e1,...) {
  .Generic <<- "seVar"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

seDVar.dynExpr <- function(e1,...) {
  .Generic <<- "seDVar"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

seRVar.dynExpr <- function(e1,...) {
  .Generic <<- "seRVar"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

seCoefLM.dynExpr <- function(e1,...) {
  .Generic <<- "seCoefLM"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

seVarLM.dynExpr <- function(e1,...) {
  .Generic <<- "seVarLM"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

coefLM.dynExpr <- function(e1,...) {
  .Generic <<- "coefLM"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

varLM.dynExpr <- function(e1,...) {
  .Generic <<- "varLM"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}


## parameter tricks !!!
seMean.default<-function(y) {
  sqrt(var(y)/length(y))
}

seVar.default<-function(y) {
  sqrt(var((y-mean(y))^2)/length(y))
}

seDMean.default<-function(y1,y2,r=1) {
  sqrt(var(y1)/length(y1)+r^2*var(y2)/length(y2))
}

seDMeanG.default<-function(y1,y2) {
  n1<-length(y1)
  n2<-length(y2)
  sqrt(((n1-1)*var(y1)+(n2-1)*var(y2))/(n1+n2-2)*(1/n1+1/n2))
}
  
seDVar.default<-function(y1,y2,r=1) {
  sqrt(var((y1-mean(y1))^2)/length(y1)+r^2*var((y2-mean(y2))^2)/length(y2))
}
  
seRMean.default<-function(y1,y2,r0) {
  if (missing(r0)) r0 <- mean(y1)/mean(y2)
  sqrt(var(y1)/length(y1)+r0^2*var(y2)/length(y2))/mean(y2)
}
  
seRVar.default<-function(y1,y2,r0) {
  if (missing(r0)) r0 <- var(y1)/var(y2)
  sqrt(var((y1-mean(y1))^2)/length(y1)+r0^2*var((y2-mean(y2))^2)/length(y2))/var(y2)
}
  
## regression tricks !!!
coefLM.default<-function(model,data) {
  switch(class(model),
    formula={
      if(missing(data)) coef(lm(model))
      else coef(lm(model,data=data))
    },
    lm={
      coef(model)
    },
    summary.lm={
      coef(model)[,1]
    })
}

seCoefLM.default<-function(model,data) {
  sumlm<-switch(class(model),
    formula={
      if(missing(data)) summary(lm(model))
      else summary(lm(model,data=data))
    },
    lm={
      summary(model)
    },
    summary.lm={
      model
    })
  sumlm$coef[,2]
}

varLM.default<-function(model,data) {
  sumlm<-switch(class(model),
    formula={
      if(missing(data)) summary(lm(model))
      else summary(lm(model,data=data))
    },
    lm={
      summary(model)
    },
    summary.lm={
      model
    })
  sumlm$sigma^2
}

seVarLM.default<-function(model,data) {
  sumlm<-switch(class(model),
    formula={
      if(missing(data)) summary(lm(model))
      else summary(lm(model,data=data))
    },
    lm={
      summary(model)
    },
    summary.lm={
      model
    })
  sqrt(var(sumlm$resid^2)/length(sumlm$resid))
}

