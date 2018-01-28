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
#'
#' @importFrom stats predict
#' @export
#'
#'
plot.optiPlusModel <- function(x, type, digits = 2, color = "#FF8000"){
  ##Obs - Pred
  if(type == "obsPred"){
    dp <- data.frame(yp = x$yp, y = x$y)
    a <- cor(dp$y, dp$yp)
    b <- mean(dp$y) - a*mean(dp$yp)
    borne <- range(dp$y)
    inf <- borne[1] - 10000
    sup <- borne[2] + 10000
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
      addTitle(text = "Predicted VS Observed", size = 20),
      addTrendLine(initialValue = pt1[1], initialXValue = pt1[2],
                   finalValue = pt2[1], finalXValue = pt2[2], lineColor = "#000000"),
      setExport()
    ))
  }
  if(type == "importance"){
    res <- data.frame(Var = row.names(x$model$importance), Imp =x$model$importance[, 1])
    res <- res[order(res$Imp, decreasing = TRUE),]
    amBarplot("Var", "Imp", res, horiz = TRUE,
              main = "Variable Importance", export=TRUE,  ylim = c(0, max(res$Imp)),
              groups_color = color)
  }
}
