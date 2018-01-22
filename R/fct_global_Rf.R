rf <- function(x, y, cvcol, ntree = 50, mtry=if (!is.null(y) && !is.factor(y))
  max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))), maxnodes = NULL, nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
  importance =FALSE, criteria = "RMSE"){

  param <- expand.grid(ntree= ntree, mtry = mtry, maxnodes = maxnodes, nodesize = nodesize)

  res <- apply(param, 1, function(z){
    z <- data.frame(t(z))

    bibounou <- .predRF(x, y, cvcol, ntree = z$ntree, mtry = z$mtry
                        , maxnodes = z$maxnodes, nodesize = z$nodesize, importance = TRUE)

    return(.compute_criteria(bibounou, criteria))
  }
  )

  bestparam <- param[which.min(res),]

  mod <- .predRF(x, y, cvcol, ntree = bestparam$ntree, mtry = bestparam$mtry,
                 maxnodes = bestparam$maxnodes, nodesize = bestparam$nodesize, importance = TRUE)

  crit1 <- .compute_criteria(mod, "RMSE")
  crit2 <- .compute_criteria(mod, "R2")
  sortie <- list(RMSE = crit1, R2 = crit2)
  return(sortie)
}
