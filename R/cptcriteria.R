#' @importFrom AUC auc roc
.compute_criteria <- function(listPred, criterion){
  #calcul pour R² cross valide :

  #criteres
  if(criterion == "RMSE"){
    RMSE = sqrt(mean((listPred$y-listPred$yp)^2))
    sortie <- RMSE
  }
  if(criterion == "R2"){
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
  if(criterion == "AUC"){
    AUC = auc(roc( listPred$prob[,2],listPred$y))
    sortie <- AUC
  }
  return(sortie)
}

.selectBestMod <- function(critoptim, criterion){
  if( criterion == "RMSE"){
    nummod <- which.min(critoptim)
  }
  if( criterion == "R2"){
    nummod <- which.max(critoptim)
  }
  if( criterion == "AUC"){
    nummod <- which.max(critoptim)
  }
  nummod
}

.addcrit <- function (criterion, modoptim){
  if (criterion == "RMSE"| criterion =="R2"){
    RMSE <- .compute_criteria(modoptim, "RMSE")
    R2 <- .compute_criteria(modoptim, "R2")
    modoptim$RMSE <- RMSE
    modoptim$R2 <- R2
  }
  if(criterion == "AUC"){
    AUC <- .compute_criteria(modoptim, "AUC")
    modoptim$AUC <- AUC
  }
  modoptim
}
