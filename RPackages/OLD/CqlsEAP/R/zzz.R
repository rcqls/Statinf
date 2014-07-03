.onLoad<- function(lib,pkg) {
  #require(getattr)
  local({ ##IF PROBLEM SEE CqlsEAP/R/zzz.R
    if(!exists(".randomEnv")) .randomEnv<-new.env(parent=.GlobalEnv)
    if(is.null(.randomEnv$.Par.EAP)) .randomEnv$.Par.EAP <- list(cex=1.5,sel.cex=2.5*1.5,sel.col="green",m.min=20)
  },.GlobalEnv)
}
