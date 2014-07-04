## TODO :
## BUGS : quand YY n'est pas initialisé Est(mu) n'est pas explicite!!
.randomEnv<-new.env()

###################################################################################
## TODO : Aller plus loin en développant le calcul de proba relativement simple
## à faire. Intégration du Support Supp(Y) pour connaitre le type de la variable Y
## Intégrer la linéarité de l'espérance EE(Y+3) = EE(Y) + 3, VV(Y+3)=VV(Y).

## PP pourra correspondre à la vraie proba d'un événement
## FF à la fonction de répartition
## Tout ceci sera facilité par la connaissance de distrib
## Peut-être qu'il faut mieux dissocier distrib de random !!!
## 1) distrib :  ayant les méthodes EE, PP, FF
## 2) random : ayant la méthode Random (ou sim)
###################################################################################



#####################################################################################################
##          class dynExpr
#####################################################################################################
## no particular constructor!!!
## combinaison des expressions aléatoires !!! => mean.dynExpr doit  se comporter comme Ops.dynExpr et Math.dynExpr !!!
## il doit y avoir un mode "construction" et un mode "évaluation"
## class dynExpr
## list(expr=chaine)
## en mode "parse" ou "construction" (mode par défaut) la chaine $expr est construite assez simplement 
## en mode "numeric" ou "évaluation" le comportement dépend de la classe de l'objet :
## pour le mode "numeric", il faut dans as.double.random (i.e. as.double) utiliser la valeur de .remember
## Random.random et print.random devront modifier .randomRemember en début et en fin de fonction !!!
## But : la classe "parameter" doit pouvoir être par la suite combinée avec la classe "random".
#####################################################################################################

as.dynExpr <- function(obj,expr=NULL) {
  if(!is.null(expr)) obj[["expr"]] <- expr
  else {
    obj[["key"]]<-findKey() #in order to convert later it in obj[["expr"]] 
   }    
  class(obj) <- unique(c("dynExpr",class(obj)))
  obj
}

## find some key in the field 
findKey<-function(env=curEnvir()) {
  objs<-ls(env=env)
  l<-sapply(objs,function(o) {o<-eval(parse(text=o),env=env);if(inherits(o,"dynExpr") && !is.null(o[["key"]])) TRUE else FALSE}) 
  keys<-character() 
  if(length(l)) {
    objs<-objs[l]
    keys<-sapply(objs,function(o) {o<-eval(parse(text=o),env=env);o[["key"]]} )
   } 
  if(length(keys)) 
    key<-max(as.numeric(keys))+1
  else 
    key<-1
  as.character(key)
}

findObj<-function(key,env=curEnvir()) {
  objs<-ls(env=env)
  objs<-objs[sapply(objs,function(o) {o<-eval(parse(text=o),env=env);if(inherits(o,"dynExpr") && !is.null(o[["key"]]) && o[["key"]]==key) TRUE else FALSE}) ]
  if(length(objs)==0) return(NULL)#stop("key unknown!!!")
  if(length(objs)>2) stop("too much keys!!!")
  objs
}

## mode for class dynExpr : "parse" (default) and "numeric"
assign(".modeDynExpr","parse",.randomEnv)
## priority order of the dynExpr classes 
assign(".priorityExprClasses",c("dynExpr","randExpr","distrib","random","sample","estimate","parameter","withParam","multivariate"),.randomEnv)
assign(".excludeExprClasses",c("distrib","sample","estimate","parameter"),.randomEnv)
## .fieldDynExpr = "expr" (default)
assign(".fieldDynExpr","expr",.randomEnv)

setDynExprClass <- function(class0) {
  if("random" %in% class0) class0 <- unique(c("randExpr",class0))
  ## order by priority
  get(".priorityExprClasses",envir=.randomEnv)->priority
   get(".excludeExprClasses",envir=.randomEnv)->exclude
  match(class0,priority,no=0)->ii
  class0 <- priority[sort(ii[ii>0])]
  ## now exclude incompatible classes
  match(class0,exclude,no=0)->ii
  ii <- ii[ii>0]
  j <- ii[1]
  class0 <- setdiff(class0,exclude[ii>j])
  class0
}

## made for expression with same objects in some expression!!
assign(".remember",FALSE,.randomEnv)


## operators on random objects !!!
initRememberList <- function() assign(".rememberList",list(),.randomEnv)
initRememberList()

## notice that this function does not depend on Random method and only on as.double !! 
getInRememberList <- function(obj) {
  name <- obj[["expr"]]
##print(name)  
#print.default(obj) 
  if(is.null(name) && !is.null(obj[["key"]])) {
    name<-obj[["expr"]]<-findObj(obj[["key"]])
  ##remove the obj[["key"]]
    if(!is.null(name)) { 
      obj[["key"]]<-NULL 
##print(name)
    assign(name,obj,curEnvir())
   }    
  }
## do not remember if name is null or obj[["expr"]] is a valid name object 
## example: Unif() +Unif() is sum of independent Uniform
## but  Y+Y (where Y<-Unif()) is not. 
  if(is.null(name) || (!obj[["expr"]] %in% ls(env=curEnvir())) )  return(as.double(obj,remember=FALSE))
  .rememberList<-get(".rememberList",envir=.randomEnv)
  if(is.null(.rememberList[[name]])) {
    .rememberList[[name]] <- (res <- as.double(obj,remember=FALSE))
    assign(".rememberList",.rememberList,envir=.randomEnv)
    ##print( .rememberList)  
  } else {
    ## check for debug !!!
    ##cat("getInRememberList -> ok\n")
    res <- .rememberList[[name]]
    ##print(res) 
  }
  res
}

multiVariate<-function(expr) {
##print.default(expr)
  oldModeDynExpr <- get(".modeDynExpr",.randomEnv)
  assign(".modeDynExpr","numeric",.randomEnv)
##print(as.double(expr))
  res<-try(as.double(expr),TRUE)
##print(res) 
##print(length(res)) 
  if( inherits(res,"try-error") || length(res)>1)
    class(expr)<-c(class(expr),"multivariate")
#else  
#  class(expr)<-setdiff(class(expr),"multivariate")
##print(class(expr))
  assign(".modeDynExpr",oldModeDynExpr,.randomEnv)
##cat(".modeDynExpr=",get(".modeDynExpr",.randomEnv),"\n")
  expr 
}

Ops.dynExpr <- function (e1, e2)
{
  .modeDynExpr<-get(".modeDynExpr",envir=.randomEnv)
  .fieldDynExpr<- get(".fieldDynExpr",.randomEnv)
  switch(.modeDynExpr,
         parse = {
	 ##print.default(e1);print.default(e2)
           class0 <- "dynExpr"
           if(inherits(e1,"dynExpr")) {
             class0 <- c(class0,class(e1))
             if(is.null(e1[[.fieldDynExpr]])) e1 <- deparse(substitute(e1))
             else e1 <- e1[[.fieldDynExpr]]
           } else e1 <- deparse(substitute(e1))
           if(inherits(e2,"dynExpr")) {
             class0 <- c(class0,class(e2))
             if(is.null(e2[[.fieldDynExpr]])) e2 <- deparse(substitute(e2))
             else e2 <- e2[[.fieldDynExpr]]
           } else e2 <- deparse(substitute(e2))
           ##print(.fieldDynExpr)
           ##print(e1)
           ##print(e2)
           expr <- list()
           expr[[.fieldDynExpr]] <- paste("(",e1,.Generic,e2,")",sep="")
           class(expr) <- setDynExprClass(unique(class0))
          expr<-multiVariate(expr)
##cat("out.Ops:",.Generic,"->");print(expr[[.fieldDynExpr]])
           return(expr)
         },
         numeric = {
           ##cat("Ops.dynExpr (eval)\n")
          .remember<-get(".remember",.randomEnv) 
           if(.remember) {
             if(inherits(e1,"dynExpr") && (is.null(e1[[.fieldDynExpr]]))) e1[[.fieldDynExpr]] <- deparse(substitute(e1))
             if(inherits(e2,"dynExpr") && (is.null(e2[[.fieldDynExpr]]))) e2[[.fieldDynExpr]] <- deparse(substitute(e2))
           }
           value <- get(.Generic, mode = "function")(as.double(e1), as.double(e2))
           ##ERROR !!! Do not do that: .modeDynExpr <<- "parse"
           value
         })
}

## Nice : ... is available for all methods in Math group
Math.dynExpr <- function(e1,...) {
 .modeDynExpr<-get(".modeDynExpr",envir=.randomEnv) 
 .fieldDynExpr<- get(".fieldDynExpr",.randomEnv)
  switch(.modeDynExpr,
         parse = {
           ##cat("parse->");print(e1[["expr"]])
           class0 <- "dynExpr"
           expr <- list()
           parDots <- names(match.call())[-(1:2)]
           valDots <- as.character(substitute(list(...)))[-1]
           if(inherits(e1,"dynExpr")) {
             class0 <- c(class0,class(e1))
             if(is.null(e1[[.fieldDynExpr]])) e1 <- deparse(substitute(e1))
             else e1 <- e1[[.fieldDynExpr]]
           } else e1 <- deparse(substitute(e1))
           if(length(parDots)) {
             .dots <- sapply(1:length(parDots),function(i) {
               if(nchar(parDots[i])) paste(parDots[i],valDots[i],sep="=")
               else valDots[i]
               })
             expr[[.fieldDynExpr]] <- paste(.Generic,"(",e1,",",paste(.dots,collapse=","),")",sep="")
             
           } else expr[[.fieldDynExpr]] <- paste(.Generic,"(",e1,")",sep="")
           class(expr) <- setDynExprClass(unique(class0))
##cat("out.Math:",.Generic,"->");print(expr[[.fieldDynExpr]]);print(length(sim(expr,1)))
            expr<-multiVariate(expr)
           return(expr)
         },
         numeric = {
           if(.remember<-get(".remember",.randomEnv)) {
             if(inherits(e1,"dynExpr") && (is.null(e1[[.fieldDynExpr]]))) e1[[.fieldDynExpr]] <- deparse(substitute(e1))
           }
##cat("numeric->",.Generic,":");print(e1[["expr"]])
           value <-  get(.Generic, mode = "function")(as.double(e1),...)
##print.default(value);print(class(value)) 
           ##ERROR !!! .modeDynExpr <<- "parse"
           value
         })
}

Math2.dynExpr <- function(e1,digits) {
 .modeDynExpr<-get(".modeDynExpr",envir=.randomEnv) 
 .fieldDynExpr<- get(".fieldDynExpr",.randomEnv)
  switch(.modeDynExpr,
         parse = {
           ##cat("parse->");print(e1[["expr"]])
           class0 <- "dynExpr"
           expr <- list()
           parDots <- names(match.call())[-(1:2)]
           valDots <- as.character(substitute(list(...)))[-1]
           if(inherits(e1,"dynExpr")) {
             class0 <- c(class0,class(e1))
             if(is.null(e1[[.fieldDynExpr]])) e1 <- deparse(substitute(e1))
             else e1 <- e1[[.fieldDynExpr]]
           } else e1 <- deparse(substitute(e1))
           expr[[.fieldDynExpr]] <- paste(.Generic,"(",e1,",digits)",sep="")
           class(expr) <- setDynExprClass(unique(class0))
##cat("out.Math:",.Generic,"->");print(expr[[.fieldDynExpr]]);print(length(sim(expr,1)))
            expr<-multiVariate(expr)
           return(expr)
         },
         numeric = {
           if(.remember<-get(".remember",.randomEnv)) {
             if(inherits(e1,"dynExpr") && (is.null(e1[[.fieldDynExpr]]))) e1[[.fieldDynExpr]] <- deparse(substitute(e1))
           }
##cat("numeric->",.Generic,":");print(e1[["expr"]])
           value <-  get(.Generic, mode = "function")(as.double(e1),digits)
##print.default(value);print(class(value)) 
           ##ERROR !!! .modeDynExpr <<- "parse"
           value
         })
}

Summary.dynExpr <- function(e1,...) {
 .modeDynExpr<-get(".modeDynExpr",.randomEnv)
 .fieldDynExpr<- get(".fieldDynExpr",.randomEnv)
  switch(.modeDynExpr,
         parse = {
           class0 <- "dynExpr"
           expr <- list()
           parDots <- names(match.call())[-(1:2)]
           valDots <- as.character(substitute(list(...)))[-1]
           if(inherits(e1,"dynExpr")) {
             class0 <- c(class0,class(e1))
             if(is.null(e1[[.fieldDynExpr]])) e1 <- deparse(substitute(e1))
             else e1 <- e1[[.fieldDynExpr]]
           } else e1 <- deparse(substitute(e1))
           .dots <- character(0)
           if(length(parDots)) {
             for(i in 1:length(parDots)) {
                if(inherits(try(eval(parse(text=valDots[i]),env=curEnvir()),TRUE),"try-error")) next
               if(nchar(parDots[i])) .dots <- c(.dots,paste(parDots[i],valDots[i],sep="="))
               else .dots <- c(.dots,valDots[i])
             }
             ##cat(".dots");print(.dots)
           }
           if(length(.dots)) {
             expr[[.fieldDynExpr]] <- paste(.Generic,"(",e1,",",paste(.dots,collapse=","),")",sep="")
           } else expr[[.fieldDynExpr]] <- paste(.Generic,"(",e1,")",sep="")
           class(expr) <- setDynExprClass(unique(class0))
            expr<-multiVariate(expr)
            return(expr)
         },
         numeric = {
           if(.remember<-get(".remember",.randomEnv)) {
             if(inherits(e1,"dynExpr") && (is.null(e1[[.fieldDynExpr]]))) e1[[.fieldDynExpr]] <- deparse(substitute(e1))
           }
           value <-  get(.Generic, mode = "function")(as.double(e1),...)
           ## ERROR !!! .modeDynExpr <<- "parse"
           value
         })
}

var.default <- var
var <- function(obj,...) UseMethod("var")
sd.default <- sd
sd <- function(obj,...) UseMethod("sd")
mth <- moment <-  function(obj,...) UseMethod("moment")

moment.default <- function (x, k=2,centred=TRUE, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  if(centred) x <- x-mean(x)
  mean(x^k) 
}

## mean, var, sd and moment added to Summary group !!!
mean.dynExpr <- function(e1,...) {
  .Generic <<- "mean"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

var.dynExpr <- function(e1,...) {
  .Generic <<- "var"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

sd.dynExpr <- function(e1,...) {
  .Generic <<- "sd"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

moment.dynExpr <- function(e1,...) {
  .Generic <<- "moment"
  Summary.dynExpr(as.dynExpr(e1,deparse(substitute(e1))),...)
}

## extraction
"[.dynExpr"<-function(e1,...) {
  .modeDynExpr<-get(".modeDynExpr",envir=.randomEnv) 
  .fieldDynExpr<- get(".fieldDynExpr",.randomEnv)
  switch(.modeDynExpr,
    parse = {
      #cat("parse->");print(e1[["expr"]])
      class0 <- "dynExpr"
      expr <- list()
      parDots <- names(match.call())[-(1:2)]
      valDots <- as.character(substitute(list(...)))[-1]
      if(inherits(e1,"dynExpr")) {
        class0 <- c(class0,class(e1))
        if(is.null(e1[[.fieldDynExpr]])) e1 <- deparse(substitute(e1))
        else e1 <- e1[[.fieldDynExpr]]
      } else e1 <- deparse(substitute(e1))
      .dots <- sapply(1:length(parDots),function(i) {
        if(nchar(parDots[i])) paste(parDots[i],valDots[i],sep="=")
        else valDots[i]
      })
      expr[[.fieldDynExpr]]<-paste(e1,"[",paste(.dots,collapse=","),"]",sep="") 
      class(expr) <- setDynExprClass(unique(class0))
#cat("out.Math:",.Generic,"->");print(expr[[.fieldDynExpr]])
class(expr)<-unique(c(class(expr),"multivariate"))
      return(expr)
    },
    numeric = {
      if(.remember<-get(".remember",.randomEnv)) {
        if(inherits(e1,"dynExpr") && (is.null(e1[[.fieldDynExpr]]))) e1[[.fieldDynExpr]] <- deparse(substitute(e1))
      }
      #cat("numeric->");print(e1[["expr"]])
      value <-  (as.double(e1))[...]
      #ERROR !!! .modeDynExpr <<- "parse"
      value
  })
}


## c.dynExpr
c.dynExpr<-function(...,recursive=FALSE) {
  .modeDynExpr<-get(".modeDynExpr",envir=.randomEnv) 
  .fieldDynExpr<- get(".fieldDynExpr",.randomEnv)
  switch(.modeDynExpr,
    parse = {
    #cat("parse->");print(e1[["expr"]])
      class0 <- "dynExpr"
      expr <- list()
      eltDots<-list(...)
      parDots <- names(match.call())[-(1:2)] 
      valDots <- as.character(substitute(list(...)))[-1]      
      if(is.null(parDots)) parDots<-rep("",length(eltDots)) 
      .dots<-character(0)
      for(i in 1:length(eltDots)) {
        elt<- eltDots[[i]]
        if(inherits(elt,"dynExpr")) {
          class0 <- c(class0,class(elt))
          if(is.null(elt[[.fieldDynExpr]])) {
            if(nchar(parDots[i])) elt<-paste(parDots[i],valDots[i],sep="=")
            else elt<-valDots[i]
          } else elt <- elt[[.fieldDynExpr]]
        } else  {
            if(nchar(parDots[i])) elt<-paste(parDots[i],valDots[i],sep="=")
            else elt<-valDots[i]
        }
        .dots<-c(.dots,elt) 
      }  
      expr[[.fieldDynExpr]]<-paste("c.dynExpr(",paste(.dots,collapse=","),")",sep="")
      class(expr) <- setDynExprClass(unique(class0))
      ##cat("out.Math:",.Generic,"->");print(expr[[.fieldDynExpr]])
      expr<-multiVariate(expr)
      return(expr)
  },
  numeric={  
    eltDots<-list(...)
    valDots <- as.character(substitute(list(...)))[-1]
    if(.remember<-get(".remember",.randomEnv)) {
        for(i in 1:length(eltDots))
          if(inherits(eltDots[[i]],"dynExpr") && (is.null(eltDots[[i]][[.fieldDynExpr]]))) eltDots[[i]][[.fieldDynExpr]] <- valDots[[i]]
    }
##cat("DynExpr :");print(valDots)
  value<-unlist(lapply(eltDots, as.double), recursive = FALSE)
##print.default(value) 
#ERROR !!! .modeDynExpr <<- "parse"
  value
  })
}

##TODO : idem with rbind, cbind, as.vector

## empirical cumulative repartition and quantile
cumfreq <- function(x,q,na.rm=FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  mean(x<q)
}

## quantile already exists !!!

## This is required for "sim(mean(YY),10)" since YY returns a list !!!
## the first one is the same as 
mean.list <-function (x, ...) sapply(x, mean, ...)
var.list <-function (x, ...) sapply(x, var, ...)
sd.list <-function (x, ...) sapply(x, sd, ...)
moment.list <- function (x, ...) sapply(x, moment, ...)

#########################################################
## Y v.a. class distrib with parameter found in some attached environment
###############################################################
#            General Methods
#############################################################
sim <- Random <- function(obj,...) UseMethod("Random")
Random.default<-function(obj,n,...) {
    if(is.numeric(obj)) return(rep(obj,n)) else return(obj)
}
QQ <- Quantile <- function(obj,...) UseMethod("Quantile")
## PP : this is not yet implemented !!!! This could be applied to some event.
## However, PP.EAP could be developped just below. 
PP <- Prob <- function(obj,...) UseMethod("Prob")
FF <- CumProb <- function(obj,...) UseMethod("CumProb")
ff <- Density <- function(obj,...) UseMethod("Density")
EE <- Mean <- function(obj,...) UseMethod("EE")
VV <- Var <- function(obj,...) UseMethod("VV")
MM <- Moment <- function(obj,...) UseMethod("Moment")
WhatIs<-function(obj,...) UseMethod("WhatIs")
Param<-function(obj,...) UseMethod("Param")
PP.sim <- PP.EAP <- Prob.EAP <- function(obj,...) UseMethod("PP.EAP")
QQ.sim <- QQ.EAP <- Quantile.EAP <- function(obj,...) UseMethod("QQ.EAP")
FF.sim <- FF.EAP <- CumProb.EAP <- function(obj,...) UseMethod("FF.EAP")
EE.sim <- EE.EAP <- Mean.EAP<- function(obj,...) UseMethod("EE.EAP")
VV.sim <- VV.EAP <- Var.EAP <- function(obj,...) UseMethod("VV.EAP")
MM.sim <- MM.EAP <- Moment.EAP <- function(obj,k=2,...) UseMethod("Moment.EAP")
is.discrete<-function(obj,...) UseMethod("is.discrete")
## Multivariate tricks!!
Margin <- function(obj,i=1,...) UseMethod("Margin")

## default methods !!!
CumProb.default<-function(obj) {
  stop("CumProb not implemented for other classes than distrib !!!")
}
Density.default<-function(obj) {
  stop("Density not implemented for other classes than distrib!!!")
}
Quantile.default<-function(obj) {
  stop("Quantile not implemented for other classes than distrib!!!")
}
WhatIs.default<-function(obj) {
  stop("WhatIs not implemented for other classes than distrib!!!")
}

Param.default<-function(obj) {
  stop("Param not implemented for other classes than distrib!!!")
}

#######################################################################################
#                  random class (No constructor)
#######################################################################################
## class random characterized by exitence of method Random (except distrib)!!!
## class random have to be specified for obj whereas "Random(obj)(n)" is meaningfull!!!
#### each random variable could be associated with its distribution !!!
WhatIs.random<-function(obj,...) {
  if(!is.null(obj[["distrib"]])) obj[["distrib"]]
  else cat("Unknown distribution\n")
}

first.EAP <- function() {
  assign(".verbose.EAP",TRUE,.randomEnv)
  assign(".tailPlotDistrib",.001,.randomEnv)
  assign(".n.random",1,.randomEnv)
  ## mode for class dynExpr : "parse" (default) and "numeric"
  assign(".modeDynExpr","parse",.randomEnv)
  ## .fieldDynExpr = "expr" (default)
  assign(".fieldDynExpr","expr",.randomEnv)
  ## made for expression with same objects in some expression!!
  assign(".remember",FALSE,.randomEnv)
}

assign(".verbose.EAP",TRUE,.randomEnv)

## used for random class which are not randExpr class like distrib
## or random object with [["distrib"]] field  
Density.random<-function(obj,...) {
  if(is.null(obj[["distrib"]])) {
    ## make estimate (kernel or other)
    warning("Not implemented") 
  }else Density(obj[["distrib"]])
} 

Quantile.random<-function(obj,...) {
    if(is.null(obj[["distrib"]])) {
       if(get(".verbose.EAP",.randomEnv)) cat("EE approximated by EE.EAP -> ")
        QQ.EAP(obj,...,verb=get(".verbose.EAP",.randomEnv))
    } else Quantile(obj[["distrib"]],...)
} 

is.discrete.random<-function (obj,nb=100) {
  length(unique(Quantile(obj,seq(0,1,l=nb))))<nb
}

CumProb.random<-function(obj,...) {
    if(is.null(obj[["distrib"]])) {
      if(get(".verbose.EAP",.randomEnv)) cat("EE approximated by EE.EAP -> ")
      FF.EAP(obj,verb=get(".verbose.EAP",.randomEnv))
    } else CumProb(obj[["distrib"]])
}

EE.random<-function(obj) {
  if(is.null(obj[["distrib"]])) {
    if(get(".verbose.EAP",.randomEnv)) cat("EE approximated by EE.EAP -> ")
    EE.EAP(obj,verb=get(".verbose.EAP",.randomEnv))
  } else EE(obj[["distrib"]])
}

VV.random<-function(obj) {
  if(is.null(obj[["distrib"]])) {
    if(get(".verbose.EAP",.randomEnv)) cat("VV approximated by VV.EAP -> ")
    VV.EAP(obj,verb=get(".verbose.EAP",.randomEnv))
  } else VV(obj[["distrib"]])
}

Moment.random <- function(obj,k=2,...) {
  if(is.null(obj[["distrib"]])) {
    if(get(".verbose.EAP",.randomEnv)) cat("Moment approximated by Moment.EAP -> ")
    Moment.EAP(obj,k,verb=get(".verbose.EAP",.randomEnv),...)
  } else Moment(obj[["distrib"]],k,...)
}

Prob.random <- function(obj) {
  ##if(is.null(obj[["distrib"]])) {
    if(get(".verbose.EAP",.randomEnv)) cat("PP approximated by PP.EAP -> ")
    PP.EAP(obj,verb=get(".verbose.EAP",.randomEnv))
  ##} else EE(obj[["distrib"]])
}

assign(".m.EAP",10000,envir=.randomEnv)

EE.EAP.random<-function(obj,m=get(".m.EAP",.randomEnv),verbose=FALSE) {
  if(verbose) cat("m=",m," realizations used!!!\n",sep="")
  mean(Random(obj,m))
}

VV.EAP.random<-function(obj,m=get(".m.EAP",.randomEnv),verbose=FALSE) {
  if(verbose) cat("m=",m," realizations used!!!\n",sep="")
  var(Random(obj,m))
}

Moment.EAP.random <- function(obj,k=2,m=get(".m.EAP",.randomEnv),verbose=FALSE,...) {
  if(verbose) cat("m=",m," realizations used!!!\n",sep="")
  moment(Random(obj,m),k,...)
}

## Let us notice that as.logical is applied before!!! (difference with EE.EAP)
PP.EAP.random<-function(obj,m=get(".m.EAP",.randomEnv),verbose=FALSE) {
  if(verbose) cat("m=",m," realizations used!!!\n",sep="")
  mean(as.logical(Random(obj,m)))
}

FF.EAP.random<-function(obj,q,m=get(".m.EAP",.randomEnv),verbose=FALSE) {
  if(verbose) cat("m=",m," realizations used!!!\n",sep="")
  yy <- Random(obj,m)
  sapply(q,function(e) mean(yy<e))
}

QQ.EAP.random<-function(obj,p,m=get(".m.EAP",.randomEnv),names=TRUE,verbose=FALSE) {
  if(verbose) cat("m=",m," realizations used!!!\n",sep="")
  quantile(Random(obj,m),p,names=names)
}


WhatIs.random<-function(obj,...) {
  cat("random variable ")
  if(!is.null(obj[["distrib"]])) print(obj[["distrib"]])
  else cat("with unknown distribution\n")
  ## to determine !!!
}

print.random <- function(obj) print(Random(obj,1))

as.double.random <- function(obj,remember=get(".remember",.randomEnv)) {
  if(remember) {
    getInRememberList(obj)
  } else {
    #cat("as.double.random\n")
    Random(obj,get(".n.random",.randomEnv))
  }
}

######################################################################################
##      general discrete distribution 
######################################################################################
rdiscrete <- function(n,mod,count) {
  prob <- count/sum(count)
  sample(mod,n,replace=T,prob=prob)
}

ddiscrete <- function(x,mod,count) {
  prob=count/sum(count)
  p <- prob[match(x,mod)]
  p[is.na(p)] <- 0
  p
}
pdiscrete <- function(q,mod,count) {
  rang=range(mod)
  ord=order(mod)
  mod=c(-Inf,sort(mod),Inf)
  prob <- c(0,cumsum(count[ord]/sum(count)),1)
  q[q<rang[1]] <- -Inf
  q[q>rang[2]] <- Inf
  prob[match(q,mod)]
}

qdiscrete <- function(p,mod,count) {
  ord=order(mod)
  mod=mod[ord]
  prob <- cumsum(count[ord]/sum(count))
  sapply(p,function(e) mod[which(prob>=e)[1]])
}


#######################################################################################
#                   distrib class
#######################################################################################
## Basic idea :
## distrib with arbitrary parameter (ex: pChapo ~> N(p,sqrt(p*(1-p)/n)) )
#######################################################################################

## useable with constructors Distribution, Normal, Student, ...
getDistrib<-function(name,call,envir=parent.frame(2)) {
  ## call is some call of the function "name" with parameters (evaluated later)
  ##here name have to be some character!!!
  
  ## parse the parameters and complete them!!!
  param<-parseParam(call,paste(c("r","p","d","q"),name,sep=""),noSelect=c("name"))
  
  obj<-list(name=name,param=names(param))
  obj <- c(obj,param)
  class(obj)<-c("distrib","random")
  obj <- as.parametrizable(obj,envir)
  obj <- as.dynExpr(obj)
  obj
}


## Distrib constructors
Distrib <- Distribution<-function(name,...) {
  name <- as.character(substitute(name))
  call<-match.call()
  getDistrib(name,call)
}


Binom<-function(...) {
  call<-match.call()
  getDistrib("binom",call)  
}

Pois <- Poisson<-function(...) {
  call<-match.call()
  getDistrib("pois",call)  
}

ChooseIn <- Discrete <- function(...) {
  call<-match.call()
  getDistrib("discrete",call)
}

Unif <- Uniform <- function(...) {
  call<-match.call()
  getDistrib("unif",call)  
}

Norm <- Normal<-function(...) {
  call<-match.call()
  getDistrib("norm",call)  
}

St <- Student<-function(...) {
  call<-match.call()
  getDistrib("t",call)
}

Chi2 <- Chisquare <- function(...) {
  call<-match.call()
  getDistrib("chisq",call)
}

## in order to fix the bug "qchisq(1,10,0)"
qchisq <- function (p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
{
    if (missing(ncp) || ncp==0) ## just add ||ncp==0
        .Internal(qchisq(p, df, lower.tail, log.p))
    else .Internal(qnchisq(p, df, ncp, lower.tail, log.p))
}

Fisher <- function(...) {
  call<-match.call()
  getDistrib("f",call)
}

## MVDistrib
getMVDistrib<-function(name,call,fcts=c("r","d"),except="n",envir=parent.frame(2)) {
  ## call is some call of the function "name" with parameters (evaluated later)
  ##here name have to be some character!!!
  
  ## parse the parameters and complete them!!!
  param<-parseParam(call,paste(fcts,name,sep=""),noSelect=c("name",except))
 
  obj<-list(name=name,param=names(param))
  obj <- c(obj,param)
  class(obj)<-c("mvdistrib","distrib","random")
  obj <- as.parametrizable(obj,envir)
  obj <- as.dynExpr(obj)
  obj
}

## MVDistrib constructors
MVDistrib <- MVDistribution<-function(name,...) {
  name <- as.character(substitute(name))
  call<-match.call()
  print(call)
  getMVDistrib(name,call)
}

require(mvtnorm)
MVNorm <- function(...) {
  call<-match.call()
  getMVDistrib("mvnorm",call)
}

MVUnif <- function(...) {
  call<-match.call()
  getMVDistrib("mvunif",call)
}

## independent case first!!!
rmvunif <- function(n,center=c(.5,.5),width=1) {
  if(length(width)==1 && length(center)>1) width=rep(width,length(center))
  width <- width/2
  res <- sapply(seq(along=center),function(i) runif(n,center[i]-width[i],center[i]+width[i]))
  if(length(center)>1) return(res) else return(as.vector(res))
}

dmvunif <- function(x,center=c(.5,.5),width=1) {
  if(length(width)==1 && length(center)>1) width=rep(width,length(center))
  side <- width/2
  dx <- rep(0,length(x)/length(center))
  hx <- 1/prod(width)
  if(length(center)>1) {
    if(is.vector(x) && length(x)==length(center)) x <- t(x)
    apply(x,1,function(e)  dx[e>=center-side & e<=center+side] <-hx) 
  } else {
    dx[x>=center-side & x<=center+side] <- hx
  }
  return(dx)
}

## Distrib methods
WhatIs.distrib<-function(obj,aff=TRUE,...) {
  namesAll<-c(norm="N",t="St",chisq="Chi2",f="F")
  name<-obj$name
  if(name %in% names(namesAll)) name<-namesAll[name]
  namedistrib<-paste(name,"(",paste(sapply(Param(obj,TRUE),as.character),collapse=","),")",sep="")
  if(aff) cat(namedistrib,"\n")
  else return(namedistrib)
}

Random.distrib<-function(obj,n,...) {
  if(missing(n)) {
    function(n=1,...) {
      do.call(paste("r",obj$name,sep=""),c(list(n=n),Param(obj,eval=TRUE),list(...)))
    }
  } else {
    ##l <- c(list(n=n),Param(obj,eval=TRUE),list(...))
    ##instr <- paste("r",obj[["name"]],"(",paste(paste(names(l),l,sep="="),collapse=","),")",sep="")
    ##print(instr)
    ##eval(parse(text=instr),envir=curEnvir())
    do.call(paste("r",obj[["name"]],sep=""),c(list(n=n),Param(obj,eval=TRUE),list(...)))
  }
}

CumProb.distrib<-function(obj,q,...) {
  if(missing(q)) {
    function(q,...) {
      do.call(paste("p",obj$name,sep=""),c(list(q=q),Param(obj,eval=TRUE),list(...)))
    }
  } else {
    do.call(paste("p",obj$name,sep=""),c(list(q=q),Param(obj,eval=TRUE),list(...)))
  }
}

Quantile.distrib<-function(obj,p,...) {
  if(missing(p)) {
    function(p,...) {
      do.call(paste("q",obj$name,sep=""),c(list(p=p),Param(obj,eval=TRUE),list(...)))
    }
  } else {
    do.call(paste("q",obj$name,sep=""),c(list(p=p),Param(obj,eval=TRUE),list(...)))
  }
}

Density.distrib<-function(obj,x,...) {
  if(missing(x)) {
    function(x,...) {
      do.call(paste("d",obj$name,sep=""),c(list(x=x),Param(obj,eval=TRUE),list(...)))
    }
  } else {
    do.call(paste("d",obj$name,sep=""),c(list(x=x),Param(obj,eval=TRUE),list(...)))
  }
}


## Finite population!!
pgcd2<-function(a,b) {
  if(b!= 0) return(pgcd2(b,a%%b)) 
  else return(a)
}

pgcd<-function(N) {
switch(as.character(length(N)),
  "0"=NA,
  "1"=N,
  "2"=pgcd2(N[1],N[2]),   
pgcd(c(pgcd2(N[1],N[2]),N[-(1:2)]))
  )
 }

assign(".tailPlotDistrib",.001,.randomEnv)

plot.distrib.disc<-function(obj,from,to,rect=F,add=F,lwd=3,lty=1,xlab="",ylab="",type="l",...) {
  if(missing(from)) {
      binf <- Quantile(obj,0) 
      from <- if(binf==-Inf) Quantile(obj,get(".tailPlotDistrib",.randomEnv))
  }
  if(missing(to)) {
      bsup<-Quantile(obj,1) 
      to <- if(bsup==Inf) Quantile(obj,1-get(".tailPlotDistrib",.randomEnv)) 
      else bsup
  }
  ##ATTENTION : ne fonctionne que pour pas==1!!!
  modals <-seq(from,to,by=1) ##todo: correct later by=1!!! 
  pas<-diff(modals)
  br <- c(modals[1]-pas[1]/2,modals+c(pas,pas[length(pas)])/2)
  freq <- ff(obj,modals)/diff(br)  
  X<-rep(br,rep(2,length(br)))
  Y<-c(0,rep(freq,rep(2,length(freq))),0)
   if(add) lines(X,Y,lwd=lwd,lty=lty,...)
  else plot(X,Y,type=type,xlab=xlab,ylab=ylab,lwd=lwd,lty=lty,...)
  if(rect && type=="l") {
    high<-c(freq[1],apply(cbind(freq[-1],freq[-length(freq)]),1,max),freq[length(freq)]) 
    segments(br,rep(0,length(br)),br,high,lwd=lwd,lty=lty,...)
    segments(modals,rep(0,length(modals)),modals,freq,lwd=1,lty=2,...) 
   abline(h=0,lwd=1,lty=lty,...) 
  }
}

plot.distrib.cont<-function(obj,...) {
  curve(Density(obj,x),...)
}

plot.distrib <- function(obj,...) {
  discrete<-is.discrete(obj) 
  l <- list(...)
  if(is.null(l$a) || !l$a) {## l$a means l$add
    argsCurve <- formalArgs("curve")
    ## complete the argument's names
    names(l) <- sapply(names(l),function(nm)
                       if(nchar(nm)) {
                         res <- argsCurve[pmatch(nm,argsCurve)]
                         if(is.na(res)) res <- nm
                         res
                       } else nm
                       )
    if(!is.null(l$from) || !is.null(l$to)) {
      if(discrete)  plot.distrib.disc(obj,...)
      else  plot.distrib.cont(obj,...)
    } else {
      rang <- Quantile(obj,c(0,1)) 
      from <- if(rang[1]==-Inf) Quantile(obj,get(".tailPlotDistrib",.randomEnv)) 
      else rang[1]
      to <- if(rang[2]==Inf) Quantile(obj,1-get(".tailPlotDistrib",.randomEnv)) 
      else rang[2]
      if(discrete)   plot.distrib.disc(obj,from=from,to=to,...)
      else  plot.distrib.cont(obj,from=from,to=to,...)
    }
  } else {
     if(discrete)  plot.distrib.disc(obj,...)
      else  plot.distrib.cont(obj,...)
  }
}

## Not the final name !!!
area <- function(obj,...) UseMethod("area")

area.distrib<-function(obj,x,prob,l=100,...) {
  if(missing(x)) {
    x <- Quantile(obj,prob)
  }
  x <- sort(x)
  if(length(x)>2) x <- x[1:2]
  if(x[1]==-Inf) x[1]<-Quantile(obj,get(".tailPlotDistrib",.randomEnv))
  if(x[2]==Inf) x[2]<-Quantile(obj,1-get(".tailPlotDistrib",.randomEnv))
 if(is.discrete(obj)) {
  ##ATTENTION : ne fonctionne que pour pas==1!!! 
  modals <-seq(x[1],x[2],by=1) ##todo: correct later by=1!!! 
  if(length(modals)==1) {
    pas<-1 #todo
    br<- modals+c(-1,1)*pas/2 
  } else {
    pas<-diff(modals)
    br <- c(modals[1]-pas[1]/2,modals+c(pas,pas[length(pas)])/2)
  }
  freq <- ff(obj,modals)/diff(br) 
  X<-rep(br,rep(2,length(br)))
  Y<-c(0,rep(freq,rep(2,length(freq))),0)
  polygon(X,Y,...) 
 } else {
  step <- diff(range(x))/l
  min <- x[1]
  max <- x[2]
  rang <- seq(min,max,length=l) 
  polygon(c(min,rang,max),c(0,Density(obj,rang),0),...)
 } 
  return(invisible())
}



EE.distrib <- function(obj) {
  name <- deparse(substitute(obj))
  distclass <- paste(obj$name,"Dist",sep="")
  call <- parse(text=paste("EE.",distclass,"(",name,")",sep=""))
  expr <- paste("EE(",name,")",sep="")
  class(obj) <- c(distclass,class(obj))
  Parameter(EE(obj),"EEParam",call,expr,lsSample(name))
}

 
VV.distrib <- function(obj,new=TRUE) {
  name <- deparse(substitute(obj))
  distclass <- paste(obj$name,"Dist",sep="")
  call <- parse(text=paste("VV.",distclass,"(",name,")",sep=""))
  expr <- paste("VV(",name,")",sep="")
  class(obj) <- c(paste(obj$name,"Dist",sep=""),class(obj))
  Parameter(VV(obj),"VVParam",call,expr,lsSample(name))
}


Param.distrib<-function(obj,eval=FALSE,...) {
  if(eval) {
    param <- obj[["param"]]
    ## retour modification à version précédente : as.numeric(...) rajoutté!!!
    ## pour résoudre : Norm(EE(Y),VV(Y)) -> pb pourquoi je l'avais modifié!!!
    ## avait été fait dans le cas où les paramètres sont des matrices
    res <- lapply(param,function(e) as.numeric(get("$.withParam")(obj,e)))
    names(res) <- param
    res
  } else obj[obj[["param"]]]
}

## Usage ci-dessus pour MVNorm dont les paramètres sont des matrices!!!
## fait pour qu'une matrice reste une matrice par transformation de as.double !!!
as.double.matrix <-  function(obj) {
  dim0<-dim(obj)
  dim1 <- dimnames(obj)
  res <- as.numeric(as.vector(obj))
  dim(res) <- dim0
  dimnames(res) <- dim1
  res
}


#######################################################################################
#                   Esp and Var
#######################################################################################
## Esperance and Variance
EE.default<-function(obj) cat("sorry, EE not implemented for",obj$name,"distribution!!\n")
VV.default<-function(obj) cat("sorry, VV not implemented for",obj$name,"distribution!!\n")
Moment.default <- function(obj) cat("sorry, Moment not implemented for",obj$name,"distribution!!\n")
EE.binomDist<-function(obj) {obj$size*obj$prob}
VV.binomDist<-function(obj) {obj$size*obj$prob*(1-obj$prob)}
EE.poisDist<-function(obj) {obj$lambda}
VV.poisDist<-function(obj) {obj$lambda}
EE.unifDist <- function(obj) {(obj$min+obj$max)/2}
VV.unifDist <- function(obj) {(obj$max-obj$min)^2/12}
EE.normDist <- function(obj) obj$mean
VV.normDist <- function(obj) {obj$sd^2}
EE.discreteDist <- function(obj) {
  prob <- obj$count/sum(obj$count)
  sum(obj$mod*prob)
}
VV.discreteDist <- function(obj) {
  prob <- obj$count/sum(obj$count)
  sum(obj$mod^2*prob)-sum(obj$mod*prob)^2
}
EE.tDist <- function(obj) 0
VV.tDist <- function(obj) {
  if(obj$df>2) obj$df/(obj$df-2)
  else Inf
}
EE.chisqDist <- function(obj) obj$df
VV.chisqDist <- function(obj) 2*obj$df
EE.fDist <- function(obj) {
  if(obj$df2>2) obj$df2/(obj$df2-2)
  else Inf
}
VV.fDist <- function(obj) {
  if(obj$df2>4) 2*obj$df2^2*(obj$df1+obj$df2-2)/obj$df1/(obj$df2-2)^2/(obj$df2-4)
  else Inf
}
## to be continued!!!
#######################################################################################

#######################################################################################
#                   mixture and mvmixture classes (First job without dynamic tricks!!!)
#######################################################################################
Mixture <- function(w,...) {
  rvs <- list(...)
  obj <- list(w=w/sum(w),rvs=rvs)
  class(obj) <- c("mixture","distrib","random")
  obj
}

WhatIs.mixture <- function(obj,aff=TRUE,...) {
  res <- sapply(seq(along=obj$w),function(i) paste(round(obj$w[i],4),WhatIs(obj$rvs[[i]],FALSE),sep=" "))
  res <- paste(res,collapse=" + ")
  if(aff) cat(res,"\n")
  else return(res)
}

Random.mixture <- function(obj,n,...) {
  y <- sample(1:length(obj$w),n,repl=TRUE,prob=obj$w)
  x <- y
  lapply(unique(y),function(mod) x[y==mod] <<- sim(obj$rvs[[mod]],sum(y==mod)))
  return(x)
}

CumProb.mixture<-function(obj,q,...) {
  res <- sapply(seq(along=obj$w),function(i) CumProb(obj$rvs[[i]],q)*obj$w[i])
  if(length(q)>1) return(apply(res,1,sum)) else return(sum(res))
}

Quantile.mixture<-function(obj,p,...) {
  Quantile.EAP(obj,p)
}

Density.mixture<-function(obj,x,...) {
 res <- sapply(seq(along=obj$w),function(i) Density(obj$rvs[[i]],x)*obj$w[i])
 if(length(x)>1) return(apply(res,1,sum)) else return(sum(res))
}

EE.mixture <- function(obj) {
  sum(sapply(seq(along=obj$w),function(i) EE(obj$rvs[[i]])$value*obj$w[i]))
}

VV.mixture <- function(obj) {
   sum(sapply(seq(along=obj$w),function(i) (VV(obj$rvs[[i]])$value+EE(obj$rvs[[i]])$value^2)*obj$w[i]))-EE(obj)^2
}

MVMixture <- function(w,...) {
  rvs <- list(...)
  obj <- list(w=w,rvs=rvs)
  class(obj) <- c("mvmixture","random")
  obj
}

Random.mvmixture <- function(obj,n,...) {
  y <- sample(1:length(obj$w),n,repl=TRUE,prob=obj$w)
  x <- matrix(0,nr=n,nc=length(sim(obj$rvs[[1]],1)))
  lapply(unique(y),function(mod) x[y==mod,] <<- sim(obj$rvs[[mod]],sum(y==mod)))
  return(x)
}

###############################################"
## Vector of Independent variables 
MVIndMarg <- function(...) {
  rvs <- list(...)
  obj <- list(rvs=rvs)
  class(obj) <- c("mvim","random")
  obj
}

Margin.mvim <- function(obj,i=1) {
  return(obj$rvs[[i]])
}

Random.mvim <- function(obj,n=1,...) {
  sapply(1:length(obj$rvs),function(i) Random(obj$rvs[[i]],n,...))
}

#######################################################################################
#                   sample class
#######################################################################################
## Pb to solve : Sample(Norm(0,sigma),10) did not work !!! NOW, it seems ok !!!
## class sample
Sample<-function(Y,size) {
  ##if(!inherits(Y,"random")) stop("Y have to be of random class")
  refY <- deparse(substitute(Y))
  callY <- parse(text=refY)
  if(missing(size)) size <- "30"
  else size <- deparse(substitute(size))
  obj<-list(rv=Y,call=callY,size=size)
  ##obj$rand<-function(n=1,...) {
  ##  env <- Envir(obj,TRUE)
  ##  size<-eval(parse(text=obj$size),env)
  ##  res<-lapply(1:n,function(i) Random(eval(obj[["call"]],env=env),size,...))
  ##  if(n==1) res<-unlist(res)
  ##  res
  ##}
  class(obj)<-c("sample","random")
  obj <- as.parametrizable(obj)
  obj <- as.dynExpr(obj)
  obj
}

WhatIs.sample<-function(obj,...) {
  env <- Envir(obj)
  size<-eval(parse(text=obj$size),env)
  oldModeDynExpr <- get(".modeDynExpr",.randomEnv)
  assign(".modeDynExpr","parse",.randomEnv)
  cat(size,"-sample of ",WhatIs(eval(obj[["call"]],env=env),FALSE),"\n",sep="")
  assign(".modeDynExpr",oldModeDynExpr,.randomEnv)
  return(invisible())
}

## obsolete soon !!
Random.sample<-function(obj,n,...) {
  if(missing(n)) return(obj$rand)
  else return(obj$rand(n,...))
}

Random.sample<-function(obj,n,...) {
  env <- Envir(obj)
  size<-eval(parse(text=obj$size),env)
  oldModeDynExpr <- get(".modeDynExpr",.randomEnv)
  assign(".modeDynExpr", "parse",.randomEnv)
  res<-lapply(1:n,function(i) Random(eval(obj[["call"]],env=env),size,...))
  assign(".modeDynExpr",oldModeDynExpr,.randomEnv)
  if(n==1) res<-unlist(res)
  res
}

length.sample <- function(obj) {
  eval(parse(text=obj$size),Envir(obj))
}

## does not work !!!
## "length<-.sample" <- function(obj,) {
##  get(obj[["size"]],Envir(obj,TRUE))
##  obj
##}
         
##################################################################
##          class parameter
##################################################################
Parameter <- function(value,class,call,expr,sample) {
  obj <- list(value=value)
  class(obj) <- c("dynExpr","parameter")
  if(!missing(class)) {
    obj[["expr"]] <- expr
    obj[["call"]] <- call
    if(length(sample)) obj[["estimate"]] <- paste(switch(class,"EEParam"="mean","VVParam"="var"),"(",sample[1],")",sep="")
    class(obj) <- c(class(obj),class)
  }
  obj
}


## Main change : this method was the previous as.double.parameter but infinite recursion occurred!! Now it seems Ok!!!
##print.parameter <- function(obj) {  
##  if(!is.null(obj[["call"]])) res <- eval(obj[["call"]],envir=curEnvir())
##  else if(!is.null(obj[["expr"]])) {
##    oldModeDynExpr <- .modeDynExpr
##    .modeDynExpr <<- "numeric"
    ##print(obj[["expr"]]) ## infinite recursion !!!
##    res <- eval(parse(text=obj[["expr"]]),envir=curEnvir())
##    .modeDynExpr <<- oldModeDynExpr
##  } else res <- obj[["value"]]
##  print(res)
##}

print.parameter <- function(obj) print(as.double(obj))

as.double.parameter <- function(obj) {
  ##cat("as.double.parameter:",obj[["expr"]],"\n")
  if(!is.null(obj[["call"]])) {
    res <- eval(obj[["call"]],envir=curEnvir())
    ##cat("as.double.parameter (call): return->",res,"\n")
    res
  } else if(!is.null(obj[["expr"]])) {
    oldModeDynExpr <- get(".modeDynExpr",.randomEnv)
    assign(".modeDynExpr","numeric",.randomEnv)
    res <- eval(parse(text=obj[["expr"]]),envir=curEnvir())
    assign(".modeDynExpr",  oldModeDynExpr,.randomEnv)
    res
  } else {
    ##cat("as.double.parameter (value): return->",obj[["value"]],"\n")
    obj[["value"]]
  }
}


WhatIs.parameter <- function(obj) {
  name <- if(!is.null(obj[["expr"]])) obj[["expr"]]
  else obj[["value"]]
  cat("Parameter :",name,"\n")
  return(invisible())
}

Est <- function(obj) UseMethod("Est")

"Est<-" <- function(obj,value) UseMethod("Est<-")

Est.parameter <- function(obj) {
  ##cat("estim:");print(obj[["estimate"]])
  oldModeDynExpr <- get(".modeDynExpr",.randomEnv)
  assign(".modeDynExpr","parse",.randomEnv)
 if(is.null(obj[["estimate"]])) {
    assign(".fieldDynExpr", "estimate",.randomEnv)
    ##cat("expr:");print(obj[["expr"]])
    res <- eval(parse(text=obj[["expr"]]),envir=curEnvir())
    ##print(str(res))
    assign(".fieldDynExpr", "expr",.randomEnv)
    estimExpr <- res[["estimate"]]
  } else estimExpr <- obj[["estimate"]]
  ##print(estimExpr)
  res <- eval(parse(text=estimExpr),envir=curEnvir())
  class(res) <- setDynExprClass(c(class(res),"estimate"))
  res<-multiVariate(res)
  assign(".modeDynExpr", oldModeDynExpr,.randomEnv)
  res
}

"Est<-.parameter" <- function(obj,value) {
  if(!inherits(value,"dynExpr")) stop("Second argument have to be a random expression\n")
  obj[["estimate"]] <- value[["expr"]]
  obj
}


lsSample <- function(ref) {
  env <- curEnvir()
  indSample<-sapply(ls(envir=env),function(nm)
  inherits(obj<-get(nm,envir=env),"sample"))
  if(length(indSample)==0)  return(character(0))  
  where <- which(indSample)
  if(!length(where)) return(character(0))
  refSample <- ls(envir=env)[where]
  ObjsSample <- lapply(refSample,function(nm) get(nm,envir=env))
  where <- which(sapply(ObjsSample,function(obj) if(!is.null(obj[["call"]]) && as.character(obj[["call"]])==ref) TRUE else FALSE))
  if(!length(where)) return(character(0))
  return(refSample[where])
} 

## current cursor for random realizations !!!
assign(".n.random",1,.randomEnv)


###############################################################################################
##                  class randExpr
###############################################################################################
## class randExpr is automatically added to the class of the object if it inherits "dynExpr" and "random"
as.double.randExpr <- function(obj,...) {
#cat("as.double.randExpr->");print(obj[["expr"]])
  ##on.exit(first.EAP())
  oldModeDynExpr <- get(".modeDynExpr",.randomEnv)
  assign(".modeDynExpr","numeric",.randomEnv)
  ###obj <- as.evalExpr(obj) 
  res <- eval(parse(text=obj[["expr"]]),env=curEnvir())
##cat("res{",obj[["expr"]],"}->");print(class(res));print(res)
##cat("\n\n")  
assign(".modeDynExpr",oldModeDynExpr,.randomEnv)
##print(get(".modeDynExpr",.randomEnv))
#as.double(res)
res
}

print.randExpr <- function(obj) {
  ##on.exit(first.EAP())
#cat("print.randExpr->");print(obj[["expr"]])
  initRememberList()
  assign(".remember", TRUE,.randomEnv)
  print(as.double(obj))
  assign(".remember", FALSE,.randomEnv)
  return(invisible())
}

Random.randExpr <- function(obj,n) {
  ##on.exit(first.EAP())
  ## just change of .n.random !!!
  ##multi <- length(obj)>1
  assign(".n.random", n, .randomEnv)
  initRememberList()
  assign(".remember", TRUE,.randomEnv)
  ## first univariate
  res <- try(as.double(obj),TRUE)
##print(res) 
  ## test if correct otherwise try multivariate
  if (inherits(res,"try-error")) {
    ##print(res);print(obj[["expr"]])
    first.EAP()
    assign(".n.random", 1, .randomEnv)
    res <- lapply(1:n,function(i) as.double(obj))
    ## test if result is some list of some vectors of length 1
    if(length(res) && all(sapply(res,length)==1)) res <- unlist(res)
  }
  assign(".remember", FALSE,.randomEnv)
  assign(".n.random", 1, .randomEnv)
  ##if(!multi) res <- unlist(res)
  res
}


Random.randExpr <- function(obj,n) {
##on.exit(first.EAP())
## just change of .n.random !!!
  assign(".n.random", n, .randomEnv)
  initRememberList()
  assign(".remember", TRUE,.randomEnv)
## first univariate
##cat("Random.randExpr : avant try\n")  
  if(n==1 || !inherits(obj,"multivariate") ) 
    res <- try(as.double(obj),TRUE)
  else {
      assign(".n.random", 1, .randomEnv) 
      res <- lapply(1:n,function(i) {
##cat("Inside random.randExpr\n") 
        initRememberList();as.double(obj)
      }) 
## test if result is some list of some vectors of length 1
    if(length(res) && all(sapply(res,length)==1)) res <- unlist(res)
  }
  assign(".remember", FALSE,.randomEnv)
  assign(".n.random", 1, .randomEnv)
##if(!multi) res <- unlist(res)
  res
}


WhatIs.randExpr <- function(obj) {
  cat("Random Expression :",obj[["expr"]],"\n")
  return(invisible())
}


## executed always before random class !!

EE.randExpr<-function(obj,verbose=get(".verbose.EAP",.randomEnv)) {
  if(is.null(obj[["distrib"]])) {
    if(verbose) cat("EE approximated by EE.EAP -> ")
    EE.EAP(obj,verb=verbose)
  } else EE(obj[["distrib"]])
}

VV.randExpr<-function(obj,verbose=get(".verbose.EAP",.randomEnv)) {
  if(is.null(obj[["distrib"]])) {
    if(verbose) cat("VV approximated by VV.EAP -> ")
    VV.EAP(obj,verb=verbose)
  } else VV(obj[["distrib"]])
}

Moment.randExpr <- function(obj,k=2,...,verbose=get(".verbose.EAP",.randomEnv)) {
  if(is.null(obj[["distrib"]])) {
    if(verbose) cat("Moment approximated by Moment.EAP -> ")
    Moment.EAP(obj,k,verb=verbose,...)
  } else Moment(obj[["distrib"]],k,...)
}

Prob.randExpr<-function(obj,verbose=get(".verbose.EAP",.randomEnv)) {
  ##if(is.null(obj[["distrib"]])) {
    if(verbose) cat("PP approximated by PP.EAP -> ")
    PP.EAP(obj,verb=verbose)
  ##} else EE(obj[["distrib"]])
}

Quantile.randExpr<-function(obj,p,...,verbose=get(".verbose.EAP",.randomEnv)) {
  if(is.null(obj[["distrib"]])) {
    if(verbose) cat("QQ approximated by QQ.EAP -> ")
    QQ.EAP(obj,p,...,verb=verbose)
    } else QQ(obj[["distrib"]],p,...)
}

#####################################################
##   Linear model
#####################################################
##lm.sim <- function(formula,eps,data) {
##sim(model.matrix(~R+P,data=champ)%*%1:3+Sample(Norm(0,1),nrow(champ)),2)
##}
##coef.lm.sim<-function(obj) {
##}




#################################################
##  Parameters tricks
##################################################
## .curEnvir tools
##################################################
assign(".curEnvir",globalenv(),envir=.randomEnv)

curEnvir <- function(env) {
  if(missing(env)) return(get(".curEnvir",envir=.randomEnv))
  assign(".curEnvir",env,envir=.randomEnv)
  return(invisible())
}

##################################################################
#       parseParam function
##################################################################
## callExpr=call
## commonFcts=set of fct sharing the same parameters to extract
## noSelect=names of parameters to avoid in the extraction!!!
parseParam<-function(callFct,commonFcts,noSelect=c()) {
  ## parse callExpr which is some match.call !!!
  paramnames<-names(callFct)
  if(is.null(paramnames)) paramnames<-rep("",length(callFct))
  selparam<-!paramnames %in% noSelect
  selparam[1]<-FALSE ## the first parameter is related to the name!!!
  param<-lapply(as.character(callFct)[selparam],function(e) parse(text=e))
  names(param)<-paramnames[selparam]
  ## complete et reorder the param (needed by Esp and Var by instance)
  paramNames<-formalArgs(commonFcts[1])
  sapply(commonFcts[-1],function(fct) {
    paramNames<<-intersect(paramNames,formalArgs(fct))
  })
  if(length(param)>length(paramNames)) stop("too many parameters!!!")
  if(length(param)) {
    if(is.null(names(param)->paramnames)) paramnames<-rep("",length(param))
    ## complete the name of the parameters!!
    sapply(1:length(param),function(i) {
      if(ii<-charmatch(paramnames[i],paramNames,no=0)) {
        names(param)[i]<<-paramNames[ii]
      }
    })
    ## complete the unnamed parameters!!
    if(is.null(names(param)->paramnames)) paramnames<-rep("",length(param))
    paramchoice<-setdiff(paramNames,paramnames[nchar(paramnames)>0])
    if((sum(nchar(paramnames)==0)->npar)>length(paramchoice)) stop("not enough parameter names!!!")
    paramnames[nchar(paramnames)==0]<-paramchoice[1:npar]
    names(param)<-paramnames
    ## error if paramnames is not included paramNames
    if(length(setdiff(paramnames,paramNames))) stop("unknown parameter!!!")
  }
  ## default parameters
  paramtmp<-formals(commonFcts[1])[paramNames]
  if(length(param)<sum(nchar(as.character(paramtmp))==0)) {
      ## OLD:stop("no enough default values!!!") -> BUG when function use missing function and parameter is not to provide
      ## Ex:  St(29) because dt(x,df,ncp,...) ncp without default value from version 2.5 of R ??? 
      ## NEW: remove the rest of the parameters maybe useable inside R because of the missing() function
      paramtmp<-paramtmp[1:length(param)]
  }
  if(length(param)) {
    ##bug#sapply(1:length(param),function(i) paramtmp[[paramnames[i]]]<<-param[[i]])
    ## correction !!!
    for(i in 1:length(param)) paramtmp[[paramnames[i]]]<-param[[i]]
  }
  paramtmp
}


#####################################################################
##   as.parametrizable (sort of constructor useable in constructor)
#####################################################################
as.parametrizable <- function(obj,envir=curEnvir()) {
  if(!is.null(obj$envir)) warning("obj has already envir param!!!")
  else {
    obj$envir <- (assign(".curEnvir",envir,.randomEnv))
  }
  if(!inherits(obj,"withParam")) class(obj) <- c(class(obj),"withParam")
  else warning("obj is already of class \"withParam\"!!!")
  obj
}

"$.withParam"<-function(obj,component) {
  x <- obj[[component]]
  env <- Envir(obj)
  ## priority is given for shareable parameters
  ##if(component %in% ls(env=env)) return(get(component,envir=env))
  if(component %in% names(obj)) {
    ## added for MVUnif() Param(MVUnif())$center was a call!!!
    if(is.expression(x) || is.call(x)) {
      assign("self",obj,envir=env)
      res <- eval(x,envir=env)
      if("self" %in% ls(env=env)) rm("self",envir=env)
      return(res)
    } else x
  } else {
    return(NULL)
  }
}

"$<-.withParam"<-function(obj,component,value) {
  x <- obj[[component]]
  env <- Envir(obj)
  ## priority is given for shareable parameters
  if(component %in% ls(env=env)) assign(component,value,envir=env)
  else {
    if(component %in% names(obj)) {
      obj[[component]] <- value
    }
  }
  obj
}

## Use of environment which are the only dynamic object in R !!! 
## this is better than list !!!
## Environment trip !!!

Envir<-function(obj,...)  UseMethod("Envir")

Envir.withParam <- function(obj) {
  env <- obj[["envir"]]
  ## update .curEnvir whenever Envir(.) is used !!!
  curEnvir(env)
  return(env)
}


### test
structExpr<-function(e) {
  if(is.expression(e)) e<-as.list(e)[[1]]
  if(!inherits(e,c("call","(","{"))) {
    if(exists(as.character(e)) && inherits(eval(e),"random")) e<-as.name(paste("as.double(",as.character(e),")",sep=""))
   return(e)  
  } else {
      return(lapply(e,structExpr))
  }   
}

### test
parseRandom<-function(e) {
  if(is.expression(e)) e<-as.list(e)[[1]]
  ch<-character(length(e)) 
##print(length(e));
##cat("class(e)=");print(class(e))
if(length(e)==1) {
   ch<-as.character(e)
   if(inherits(e,c("call","<-"))) {
    ch<-as.character(parse(text=paste("'",ch[1],"'(",paste(ch[-1],collapse=","),")",sep="")))
  }
  ##declare ch as a key
  
  res<-paste("as.double(",ch,")",sep="")  
 } else {
  for(i in seq(e)) {
##cat("i=",i,"->");print(e[[i]]);print(length(e[[i]]));print(as.character(e[[i]]))
    if(length(e[[i]])>1) {  
      ch[i]<-parseRandom(e[[i]])
    } else {
      if(exists(as.character(e[[i]])) && inherits(eval(e[[i]]),"random")) {
          ch[i]<-paste("as.double(",as.character(e[[i]]),ifelse(inherits(e[[i]],"call"),"()",""),")",sep="")
      } 
      else ch[i]<-as.character(e[[i]]) 
    } 
  }
  if(inherits(e,c("call","<-"))) {
    res<-as.character(parse(text=paste("'",ch[1],"'(",paste(ch[-1],collapse=","),")",sep=""))) 
  } else if(inherits(e,c("{","("))) {
     end<-ifelse(ch[1]=="{","}",")")  
##print(ch);print(end);print(ch[-1])
     ch<-as.character(as.list(parse(text=paste(ch[1],paste(ch[-1],collapse=";"),end,sep="")))[[1]])
     res<- paste(ch[1],paste(ch[-1],collapse=";"),end,sep="")
  } else {
    cat("class(e)=");print(class(e))
    cat("undeclared!!!\n")  
  } 
} 
##print(res) 
 res 
}

evalRandom<-function(expr) {
  parseRandom(substitute(expr))
}


## constructor of randExpr class 
RandExpr<-function(expr) {
  obj<-list(expr=parseRandom(substitute(expr)))
  class(obj)<-c("dynExpr","randExpr","random")
  obj<-multiVariate(obj)
  obj 
} 

## obsolete now!!! since Random.randExpr do the same!!
SIM<-function(expr,n=1) {
  expr2eval<-parseRandom(substitute(expr))
##on.exit(first.EAP())
## just change of .n.random !!!
  assign(".remember", TRUE,.randomEnv)
## first univariate
##cat("Random.randExpr : avant try\n")  
  assign(".n.random", 1, .randomEnv) 
  res <- lapply(1:n,function(i) {
##cat("Inside random.randExpr\n") 
    initRememberList();eval(parse(text=expr2eval),env=curEnvir())
  }) 
## test if result is some list of some vectors of length 1
  if(length(res) && all(sapply(res,length)==1)) res <- unlist(res)
  assign(".remember", FALSE,.randomEnv)
  assign(".n.random", 1, .randomEnv)
##if(!multi) res <- unlist(res)
  res
}
