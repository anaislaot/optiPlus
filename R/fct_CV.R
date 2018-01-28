#' @title Creates a cross validation column
#'
#'
#' @description createCv permit to create a cross validation column by specifiying one or more
#' columns of the dataset, or a number of folds.
#'
#' @param data \code{data.frame},  A dataset.
#' @param columName \code{vector}, A character vector of the name of one or more columns of the dataset.
#' @param kfolds \code{numeric}, Number of folds.
#' @param seed \code{numeric}, A seed to specify. By default there is no seed.
#'
#' @return A vector with cross-validation fold index assignment per observation
#'
#' @examples
#'
#' data(mtcars)
#'
#' createCv(mtcars, columName = "cyl")
#' createCv(mtcars, columName = c("cyl", "carb"))
#' createCv(mtcars, kfolds = 4)
#' createCv(mtcars, kfolds = 4, seed = 1234)
#'
#'
#' @export
#'
#'

createCv <- function(data, columName = NULL, kfolds = NULL, seed = NULL){
  if(is.null(columName) & is.null(kfolds)){
    stop("You must specify a columName or a kfolds")
  }
  if(!is.null(columName) & !is.null(kfolds)){
    stop("You must choose between columName or kfolds")
  }
  if(!is.null(seed)){
    set.seed(seed)
  }
  if(!is.null(columName)){
    cv <- apply(data[,columName, drop = FALSE], 1, function(X){
      paste(X, collapse = "_")})
  }else{
    if(kfolds == nrow(data)){
      cv <- 1:nrow(data)
    }else{
    cv <- sample.int(kfolds, nrow(data),replace = TRUE)
    }
  }
  cv
}

