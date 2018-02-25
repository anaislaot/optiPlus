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
#' @param criterion \code{character}, Criterion used to select the best model among the grid of hyperparameters.It can be : "RMSE", "R2", "MAPE" or "AUC".
#'
#' @return A list containing :
#'
#'   \itemize{
#'     \item{response variable (y)}
#'     \item{predicted values (yp)}
#'     \item{the cross-validation column (cvcol)}
#'     \item{the optimized parameters (ntree, mtry, maxnodes, nodesize)}
#'     \item{the criteria (RMSE, R2, MAPE, AUC...)}
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
rfMod <- function(x, y, cvcol, ntree = 500, mtry=if (!is.null(y) && !is.factor(y))
  max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))), maxnodes = NULL, nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
  criterion = "RMSE", nbcore = NULL){

  if(is.null(maxnodes)){maxnodes <- NA}
  param <- expand.grid(ntree= ntree, mtry = mtry, maxnodes = maxnodes, nodesize = nodesize)

  if(!is.null(nbcore)){
  cl <- makeCluster(nbcore)
  clusterEvalQ(cl, expr = {
    require(optiPlus)
    require(randomForest)
  })
  clusterExport(cl,
                varlist = c("x", "y", "cvcol"), envir = environment())
  }else{
    cl = NULL
  }
  #boucle qui calcul tous les modeles en fonction des parametres de la grille
  modelList <- apply(param, 1, function(z){
    z <- data.frame(t(z))
    ntree = z$ntree
    mtry = z$mtry
    maxnodes = z$maxnode
    nodesize = z$nodesize
    if(!is.null(nbcore)){
    clusterExport(cl,
                  varlist = c("ntree","mtry", "maxnodes", "nodesize"), envir = environment())
    }

    model <- .predRF(x = x, y = y, cvcol = cvcol, ntree = z$ntree, mtry = z$mtry
                     , maxnodes = z$maxnodes, nodesize = z$nodesize, cl = cl)
    model$param <- z #pour recuperer les parametres associés au modèle
    return(model)
  }
  )

  if(!is.null(nbcore)){
  stopCluster(cl)
  }
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
  cl = NULL){

  if(!is.null(maxnodes)){if(is.na(maxnodes)){maxnodes <- NULL}}

  if(!is.null(cl)){
  model <- parSapply(cl, unique(cvcol), function(d){
    mod.rf <- randomForest(x = x[which(cvcol!=d), ], y = y[which(cvcol!=d)],
                           ntree = ntree, mtry = mtry, maxnodes = maxnodes,
                           nodesize = nodesize)

    if(!is.factor(y)){
      cvfitted <- predict(mod.rf, newdata = x[which(cvcol ==d),])
      return(data.frame(yp=cvfitted, ord = which(cvcol ==d)))
    }else{
      cvfitted = predict(mod.rf, newdata = x[which(cvcol==d),], type = "prob")
      cvfitted2 = predict(mod.rf, newdata = x[which(cvcol==d),], type = "response")
      return(data.frame(yp=cvfitted, cvfitted2 = cvfitted2, ord = which(cvcol ==d)))
    }
  }, simplify = FALSE)
  }else{
    model <- sapply(unique(cvcol), function(d){
      mod.rf <- randomForest(x = x[which(cvcol!=d), ], y = y[which(cvcol!=d)],
                             ntree = ntree, mtry = mtry, maxnodes = maxnodes,
                             nodesize = nodesize)

      if(!is.factor(y)){
        cvfitted <- predict(mod.rf, newdata = x[which(cvcol ==d),])
        return(data.frame(yp=cvfitted, ord = which(cvcol ==d)))
      }else{
        cvfitted = predict(mod.rf, newdata = x[which(cvcol==d),], type = "prob")
        cvfitted2 = predict(mod.rf, newdata = x[which(cvcol==d),], type = "response")
        return(data.frame(yp=cvfitted, cvfitted2 = cvfitted2, ord = which(cvcol ==d)))
      }
    }, simplify = FALSE)
  }

  model <- do.call(rbind, model)
  model <- model[order(model$ord),]
  cvfitted <- model$yp

  modglob <- randomForest(x = x, y = y,
                          ntree = ntree, mtry = mtry, maxnodes = maxnodes,
                          nodesize = nodesize, importance = TRUE)
  if(!is.factor(y)){
    ypred <- list(y = y, yp = cvfitted, cvcol = cvcol, model = modglob)
  }else{
    matconf <- table(y, model$cvfitted2)
    prob <- model[,1 : length(unique(y))]
    ypred <- list(y = y, yp = model$cvfitted2, prob = prob, cvcol = cvcol, model = modglob,
                  confMat = matconf)
  }

  ##Add a class to list of output object
  class(ypred) <- c("optiPlusModel", class(ypred))

  return(ypred)
}
