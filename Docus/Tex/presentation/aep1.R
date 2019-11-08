anniv<-function(n,toutes=TRUE) {
  mois<-c(janvier=31,fevrier=28,mars=31,avril=30,mai=31,juin=30,juillet=31,aout=31,septembre=30,octobre=31,novembre=30,decembre=31)
  cumsum(c(0,mois))->tmp
  tmp<-tmp[-length(tmp)]
  names(tmp)<-names(mois)
  tab<-table(floor(runif(n,0,365))+1)
  names(tab)<-sapply(names(tab), function(d) {
    d<-as.integer(d)
    tmp2<-(d-tmp)
    tmp3 <- (1<= tmp2 & tmp2 <=mois)
    paste(tmp2[tmp3], names(mois)[tmp3])
  })
  if(toutes) {
    system("clear")
    tab<-tab[order(paste(tab,"|",names(tab)),decreasing=T)]
    tab
  } else tab[which.max(tab)]
}

anniv.Prob.AEP<-function(n,m=10000,save=NULL) {
  tmp<-sapply(1:m,function(i) anniv(n,FALSE))
  if(!is.null(save)) {
    assign(save,tmp)
    eval(parse(text=paste("save(",save,",file=\"",save,".RData\")",sep="")))
    rm(save)
  }
  length(tmp[tmp>1])/m
}

anniv.Prob.AMP<-function(n) 1-prod((365-1:(n-1))/365)

anniv.Prob<-function(n,m=10000) {
  cat("Proba au moins 2 personnes parmi",n,"choisis au hasard:\n")
  cat("     = ",anniv.Prob.AEP(n,m)*100,"% via l'AEP avec m=",m,"\n",sep="")
  cat("     = ",anniv.Prob.AMP(n)*100,"% via AMP (comme AEP avec m=infini)\n",sep="")
}

#############################

# Autre Gobelet à choisir au second tour
gobeletChoixSecondTour<-function(gob,choix,nb=3) {
  if(gob==choix) sample(setdiff(1:nb,gob),1) else gob
}

gobelet<-function(change=TRUE,nb=3,aff=TRUE,pause=TRUE) {
  gob<-sample(nb,1,replace=TRUE)
  choix<-sample(nb,1,replace=TRUE)
  if(aff) {
    system("clear")
    cat("Choix au premier tour\t: ")
    for(i in 1:nb) if(i==choix) cat(" [",i,"] ",sep="") else cat("  ",i,"  ",sep="")
    if(pause) readline() else cat("\n")
  }
  autreChoix<-gobeletChoixSecondTour(gob,choix,nb)
  mauvaisChoix<-setdiff(1:nb,c(autreChoix,choix))
  choix2<-if(change) autreChoix else choix
  if(aff) {
    cat(nb-2," gobelets retournés\t: ",sep="")
    for(i in 1:nb) 
      if(i==choix)                 cat(" [",i,"] ",sep="") 
      else if(i %in% mauvaisChoix ) cat("  X  ")
      else                          cat("  ",i,"  ",sep="")
    if(pause) readline() else cat("\n")
    cat("Choix joueur ",if(change) "changé\t: " else "inchangé\t: ",sep="")
    for(i in 1:nb) 
      if(i==choix2)                 cat(" [",i,"] ",sep="") 
      else if(i %in% mauvaisChoix ) cat("  X  ")
      else                          cat("  ",i,"  ",sep="")
    if(pause) readline() else cat("\n")
    cat("On retourne tout\t: ")
    for(i in 1:nb) 
      if(i==choix2)                 cat(" [",if(i==gob) "O" else "X" ,"] ",sep="") 
      else cat("  ",if(i==gob) "O" else "X","  ",sep="")
    cat("\n")
  }
}

gobelet.Prob<-function(m=100,change=TRUE,nb=3) {
  gob<-sample(nb,m,replace=TRUE)
  choix<-sample(nb,m,replace=TRUE)
  cat("Proba de choisir le bon gobelet au premier choix\n")
  cat("     = ",mean(gob==choix)*100,"% via l'AEP avec m=",as.integer(m),"\n",sep="")
  cat("     = ",1/nb*100,"% via AMP (comme AEP avec m=infini)\n",sep="")
  gobAutre<-sapply(1:m,function(i) gobeletChoixSecondTour(gob[i],choix[i],nb))
  choix2<-if(change) gobAutre else choix
  cat("Proba de choisir le bon gobelet au deuxième choix (stratégie=",ifelse(change,"choix changé","même choix"),")\n")
  cat("     = ",mean(choix2==gob)*100,"% via l'AEP avec m=",as.integer(m),"\n",sep="")
  cat("     = ",ifelse(change,(nb-1)/nb*100,1/nb*100),"% via AMP (comme AEP avec m=infini)\n",sep="")
}
