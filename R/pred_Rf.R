.predRF <- function(x, y, cvcol, ntree = 50, mtry=if (!is.null(y) && !is.factor(y))
  max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))), maxnodes = NULL, nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
  importance =FALSE){

  if(!is.factor(y)){
    n = nrow(x)
    cvfitted = rep(0,n)
  }else{
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
  return(ypred)
}
