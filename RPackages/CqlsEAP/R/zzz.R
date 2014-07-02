.onAttach <- function(lib,pkg) {
    if(is.null(.randomEnv$.Par.EAP)) .randomEnv$.Par.EAP <- list(cex=1.5,sel.cex=2.5*1.5,sel.col="green",m.min=20)
}
