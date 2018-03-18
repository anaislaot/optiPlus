#' @title Plot for optiPlusModel class
#'
#' @description plot creates plots for optiPlusModel class according to the specified type
#'
#'
#' @param x \code{optiPlusModel}. An object of class optiPlusModel.
#' @param type \code{character}. To choose the type of plot :
#'   \itemize{
#'     \item{"obsPred" : plot Predicted vs Observed}
#'     \item{"importance" : plot of Variable Importance}
#'   }
#' @param digits \code{numeric}. integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#' @param color \code{character}. Choose color in hexadecimal.
#'
#' @import rAmCharts pipeR
#' @examples
#'
#' data(mtcars)
#'
#' #Creation of cross-validation column :
#' cv <- createCv(mtcars, columName = "cyl")
#'
#' #Data
#' y <- "mpg"
#' ycolumnindex <- names(mtcars) == "mpg"
#' x <- mtcars[, !ycolumnindex]
#' y <- mtcars[, ycolumnindex]
#'
#' mod <- rfMod(x = x, y = y, cvcol = cv,
#'  ntree= c(50, 100), mtry = c(3,4),
#'   nodesize = c(3, 4, 5),  criterion = "RMSE")
#'
#' plot(mod, type = "obsPred", digits = 3, color = "#00b300")
#' plot(mod, type = "importance", color = "#00b300")
#' plot(mod, type = "decileProb")
#'
#' @importFrom stats predict
#' @export
#'
#'
plot.optiPlusModel <- function(x, type, digits = 2, color = "#4d88ff"){
  ##Obs - Pred
  if(type == "obsPred"){
    dp <- data.frame(yp = x$yp, y = x$y)
    a <- cor(dp$y, dp$yp)
    b <- mean(dp$y) - a*mean(dp$yp)
    borne <- range(dp$y)
    inf <- borne[1]
    sup <- borne[2]
    pt1 <- inf*a + b
    pt2 <-  sup*a + b
    pt1 <- c(pt1, inf)
    pt2 <- c(pt2, sup)

    dp$yp <- round(dp$yp, digits)
    dp$y <- round(dp$y, digits)

    ##Plot
    return(pipeR::pipeline(
      amXYChart(dataProvider = dp),
      addGraph(balloonText = 'Y pred:<b>[[yp]]</b> Y :<b>[[y]]</b>',
               bullet = 'circle', lineAlpha=0, xField = 'yp',yField = 'y', maxBulletSize = 100,
               lineColor = color),
      setChartCursor(),
      addValueAxes(title = "Predicted values"),
      addValueAxes(title = "Observed values", position = "bottom"),
      addTitle(text = "Predicted VS Observed", size = 15),
      addTrendLine(initialValue = pt1[1], initialXValue = pt1[2],
                   finalValue = pt2[1], finalXValue = pt2[2], lineColor = "#000000"),
      setExport()
    ))
  }
  if(type == "importance"){
    res <- data.frame(Var = row.names(x$model$importance), Imp =x$model$importance[, 1])
    res <- res[order(res$Imp, decreasing = TRUE),]
    return(amBarplot("Var", "Imp", res, horiz = TRUE,
              main = "Variable Importance", export=TRUE,  ylim = c(0, max(res$Imp)),
              groups_color = color))
  }

  if(type == "decileProb"){

    data <- data.frame(prob = x$prob[2], y = x$y)

      nb0 = rep(0, 10)
      nb1 = rep(0, 10)
      nb = 1
      seuil = seq(0, 0.9, by = 0.1)
      names(data) <- c("prob", "y")
      lev <- levels(data$y)
      label = NULL

      for (i in seuil){
        if(i == 0){
          vect <- which(data$prob >= i & data$prob <= (i + 0.1))
          nbdec <- length(vect)
          nb0[nb] <- length(which(data[vect,]$y == lev[1]))*100/ nbdec
          nb1[nb] <- length(which(data[vect,]$y == lev[2]))*100/ nbdec
          label[nb] <- paste(i, "-", (i+0.1))
        }
        vect <- which(data$prob > i & data$prob <= (i + 0.1))
        nbdec <- length(vect)
        nb0[nb] <- length(which(data[vect,]$y == lev[1]))*100/ nbdec
        nb1[nb] <- length(which(data[vect,]$y == lev[2]))*100/ nbdec
        label[nb] <- paste(i, "-", (i+0.1))

        nb <- nb +1
      }

      nb0[is.nan(nb0)] <- 0
      nb1[is.nan(nb1)] <- 0

      sortie <- data.frame(nb0 = round(nb0, 2) , nb1 = round(nb1, 2), label = label)
      title1 <- paste("Obs :", lev[1])
      title2 <- paste("Obs :", lev[2])

      return(pipeR::pipeline(
        amSerialChart(categoryField = 'label'),
        setDataProvider(sortie),
        addGraph(balloonText = '<b>[[category]]: [[value]]</b>', type = 'column',
                 valueField = 'nb0', fillAlphas = 1, lineAlpha = 0, title = title1, fillColors = '#FF00FF'),
        addGraph(balloonText = '<b>[[category]]: [[value]]</b>',type = 'column',
                 valueField = 'nb1', fillAlphas = 1, lineAlpha = 0, title = title2),
        addValueAxes(stackType = 'regular', maximum = 100),
        setCategoryAxis(position = "bottom", title = 'Probability by deciles', labelRotation = 45  ),
        setChartCursor(),
        setLegend(position = 'bottom' ,useGraphSettings = TRUE, valueWidth = 100)
      ))
  }

  if(type == "density"){
    data <- data.frame(prob = x$prob[2], y = x$y)
    names(data) <- c("prob", "y")
    return(.ggplot2.density(data=data, xName='prob', groupName='y',
                 alpha=0.5, fillGroupDensity=TRUE, backgroundColor="white",
                 removePanelBorder=TRUE, removePanelGrid=TRUE, xtitle = "probability",
                 axisLine = c(0.5, "solid" ,"darkblue"),
                 groupColors = c("#e73b27", "#2b2b2a")))
  }

  if(type == "ROC"){
    data <- data.frame(prob = x$prob[2], y = x$y)
    names(data) <- c("prob", "y")
    dataRoc <- roc(data$prob, data$y)
    dataRoc <- data.frame(x = dataRoc$fpr, y = dataRoc$tpr)

    return(amPlot(x= dataRoc$x, y= dataRoc$y, type = "l", xlab = "fpr", ylab = "tpr", main = "ROC Curve",
           export = TRUE, col = "#d32144", creditsPosition = "bottom-right")%>>%
      addTrendLine(initialValue = 0, initialXValue = 0,
                   finalValue = 1, finalXValue = 1, lineColor = "#000000"))

  }

  if(type == "residualPlot"){
    resid <- x$y - x$yp

    return(amHist(resid, xlab = "Residuals", ylab = "", main = "Residuals plot", creditsPosition = "bottom-right",
           col = "#940aed"))
  }

  if(type == "Matconf"){
    return(.amHeatmap(as.data.frame.matrix(x$confMat),  col=c("#ffffff","#ed370a"), cex = 30))

  }

}




