#' @title Launch a RandomForest with cross-validation and the possibility of tuning hyperparameters
#'
#'
#' @description rfMod uses the {randomForest} function of the package {randomForest}. rfMod allows to launch a random forest
#' for classification and regression while choosing a column of cross-validation and specifying a grid of hyperparameters.
#'
#' @param x \code{data.frame},  Predictor variables.
#' @param y \code{vector}, Response variable.
#' @param cvcol \code{vector}, Column with cross-validation fold index assignment per observation.
#' @param ntree \code{numeric}, Number of trees to grow. Default is 50.
#' @param mtry \code{numeric}, Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3).
#' @param maxnodes \code{numeric}, Maximum number of terminal nodes trees in the forest can have. If not given, trees are grown to the maximum possible (subject to limits by nodesize).
#' @param nodesize \code{numeric}, Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). Default is 5.
#' @param importance \code{numeric}, Should importance of predictors be assessed? Default is FALSE.
#' @param criterion \code{character}, Criterion used to select the best model among the grid of hyperparameters.It can be : "RMSE", "R2" or "AUC".
#'
#' @return A list containing :
#'
#'   \itemize{
#'     \item{response variable (y)}
#'     \item{predicted values (yp)}
#'     \item{the cross-validation column (cvcol)}
#'     \item{the optimized parameters (ntree, mtry, maxnodes, nodesize)}
#'     \item{the criteria (RMSE, R2, AUC...)}
#'   }
#'
#' @examples
#'
#' data(mtcars)
#'
#' #Creation of cross-validation column :
#' set.seed(1234)
#' cv <- sample(1:8, nrow(mtcars), replace = TRUE)
#'
#' #Data
#' y <- "mpg"
#' ycolumnindex <- names(mtcars) == "mpg"
#' x <- mtcars[, !ycolumnindex]
#' y <- mtcars[, ycolumnindex]
#'
#' rfMod(x = x, y = y, cvcol= cv,
#'  ntree= c(50, 100), mtry = c(3,4),
#'   nodesize = c(3, 4, 5),  criterion = "RMSE")
#'
#'
#' @importFrom stats predict
#' @export
#'
#'
rfMod<- function(x, y, cvcol, ntree = 50, mtry=if (!is.null(y) && !is.factor(y))
  max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))), maxnodes = NULL, nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
  importance =FALSE, criterion = "RMSE"){

  if(is.null(maxnodes)){maxnodes <- NA}
  param <- expand.grid(ntree= ntree, mtry = mtry, maxnodes = maxnodes, nodesize = nodesize)

  #boucle qui calcul tous les modeles en fonction des parametres de la grille
  modelList <- apply(param, 1, function(z){
    z <- data.frame(t(z))

    model <- .predRF(x = x, y = y, cvcol = cvcol, ntree = z$ntree, mtry = z$mtry
                     , maxnodes = z$maxnodes, nodesize = z$nodesize, importance = TRUE)
    model$param <- z #pour recuperer les parametres associés au modèle
    return(model)
  }
  )

  #boucle qui calcul tous le critere d'optim pour chaque modele
  critoptim <- unlist(lapply(modelList, function(x){
    .compute_criteria(x, criterion)
  }))

  #selectionner le meilleur modele au vu du critere choisi
  modoptim <- .selectBestMod(critoptim, criterion)
  modoptim <- modelList[[modoptim]]

  #relancer calcul RMSE, R², AUC..
  modoptim <- .addcrit(criterion, modoptim)
  modoptim
}

#' @importFrom randomForest randomForest
.predRF <- function(x, y, cvcol, ntree = 50, mtry=if (!is.null(y) && !is.factor(y))
  max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))), maxnodes = NULL, nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
  importance =FALSE){

  if(!is.null(maxnodes)){if(is.na(maxnodes)){maxnodes <- NULL}}
  #gerer regression ou classif
  if(!is.factor(y)){
    n = nrow(x)
    cvfitted = rep(0,n)
  }else{#cas ou en classif il y a plus de 2 classes à prédire
    n = nrow(x)
    m = nlevels(y)
    cvfitted = matrix(nrow= n, ncol= m)
    cvfitted2 = factor(rep(NA,n), levels = levels(y))
  }

  for (i in unique(cvcol)){
    mod.rf <- randomForest(x = x[which(cvcol!=i), ], y = y[which(cvcol!=i)],
                           ntree = ntree, mtry = mtry, maxnodes = maxnodes,
                           nodesize = nodesize, importance = importance)
    if(!is.factor(y)){
      cvfitted[which(cvcol==i)] = predict(mod.rf, newdata = x[which(cvcol==i),])
    }else{
      cvfitted[which(cvcol==i), ] = predict(mod.rf, newdata = x[which(cvcol==i),], type = "prob")
      cvfitted2[which(cvcol==i) ] = predict(mod.rf, newdata = x[which(cvcol==i),], type = "response")
    }
  }
  if(!is.factor(y)){
    ypred <- list(y = y, yp = cvfitted, cvcol = cvcol)
  }else{
    ypred <- list(y = y, yp = cvfitted2, prob = cvfitted, cvcol = cvcol)
  }

  class(ypred) <- c("optiPlusModel", class(ypred))

  return(ypred)
}
