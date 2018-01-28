#' Plot
#'
#' Plot
#'
#' @param x \code{optiPlusModel}.
#' @param type \code{character}.
#' @param digits \code{numeric}
#' @param color \code{character}
#'
#' @import rAmCharts pipeR
#' @export
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
    pipeR::pipeline(
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
    )
  }
}


