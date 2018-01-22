.compute_criteria <- function(listPred, criteria){
  #calcul pour R² cross valide :

  #criteres
  if(criteria == "RMSE"){
    RMSE = sqrt(mean((listPred$y-listPred$yp)^2))
    sortie <- RMSE
  }
  if(criteria == "R2"){
    if(!is.factor(listPred$y)){
      n<-length(listPred$y)
      moy <- rep(0,n)
      if(is.null(listPred$cvcol)){
        moy <- mean(listPred$y)
      }else{
        for (i in unique(listPred$cvcol)) {
          meanCV<- mean(listPred$y[which(listPred$cvcol!=i)])
          moy[which(listPred$cvcol==i)]<-meanCV
        }
      }}
    R2 = 1 - (sum((listPred$y-listPred$yp)^2)/sum((listPred$y-moy)^2))
    sortie <- R2
  }
  if(criteria == "AUC"){
    AUC = auc(roc( listPred$prob[,2],listPred$y))
    sortie <- AUC
  }
  return(sortie)
}
