#' @title Shiny app for pckage
#'
#' @description API for model
#'
#' @param data \code{data.frame}. Data
#'
#'
#' @export
shinyOptiPlus <- function(data){


  G <- .GlobalEnv
  assign("data", data, envir = G)

  shiny::runApp(system.file("shinyoptiPlus", package = "optiPlus"),
                launch.browser = TRUE)
}
