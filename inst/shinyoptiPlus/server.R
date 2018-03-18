#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(input, output, session) {

  output$hist <- renderAmCharts({

    x <- data[, input$selectDesc]
    if(is.factor(x)){
      x <- as.character(x)
    }

    y <- data[, input$selectDesc2]
    if(is.factor(y)){
      y <- as.character(y)
    }


    if (!input$var2check){
      if(!is.numeric(x)){
        data2 <- as.data.frame(table(x))
        montitre <- paste ("Barplot of", input$selectDesc)
        Graph <- amBarplot(x = "x", y = "Freq"  , data = data2, main = montitre)
      }else{
        montitre <- paste ("Histogram of", input$selectDesc)
        Graph <- amHist(x= x, xlab = input$selectDesc, ylab = "frequency", freq =TRUE,
                        export= TRUE, main = montitre)
      }
    }else{
      if(is.numeric(x) & is.numeric(y)){
        montitre <- paste ("Plot of", input$selectDesc)
        Graph <- amPlot(x = x, y = y, main = montitre)
      }

      if(is.character(x) & is.character(y)){
        data2 <- as.data.table(table(x, y))
        data2 <- dcast(data2, x~y)
        name <- as.vector(names(data2)[-1])
        ListGraph <- sapply(name, function(j){
          G <- amGraph(balloonText='<b>[[category]]: [[value]]</b>',
                       type = 'column',valueField = j, fillAlphas = 1, lineAlpha = 0, title = j)
          G
        }, simplify = FALSE, USE.NAMES = FALSE)
        Graph <- pipeR::pipeline(
          amSerialChart(categoryField = 'x'),
          setDataProvider(data2),
          setGraphs(ListGraph),
          setLegend(useGraphSettings = TRUE)
        )
      }
      if(is.numeric(x) & is.character(y)){
        formula <- as.formula(paste0(input$selectDesc, "~", input$selectDesc2))
        montitre <- paste ("Boxplot of", input$selectDesc2)

        df <- data.frame(y = y, x = x)
        names(df) <- c(input$selectDesc2, input$selectDesc)

        Graph <- amBoxplot(formula, data = df, main = montitre)
      }

      if(is.character(x) & is.numeric(y)){

        formula <- as.formula(paste0(input$selectDesc, "~", input$selectDesc2))
        montitre <- paste ("Boxplot of", input$selectDesc)
        df <- data.frame(y = y, x = x)
        names(df) <- c(input$selectDesc, input$selectDesc2)


        Graph <- amBoxplot(formula, data = df, main = montitre)
      }
      Graph
    }
  })

  #Page 2

  #Cvcol
  cvcol <- reactive({
    if (input$cvcol == 'Select a column'){
      cvcol <- createCv(data, columName = input$SelectColumn)
      return(cvcol)
    }
    if (input$cvcol == 'By kfolds'){
      cvcol <- createCv(data, kfolds = input$selectFolds)
      return(cvcol)
    }
  })

  #ntree
  ntreeReact <- reactive({
    if(!input$ntree){
      ntree <- input$selectNtree
      return(ntree)
    }else{
      ntree <- seq(input$ntreeMin, input$ntreeMax, by = input$ntreeBy)
      return(ntree)
    }
  })

  #mtry
  observe({
    if(!is.factor(data[, input$SelectY])){
      updateNumericInput(session, "selectMtry", value = round(ncol(data)/3,0))
    }else{
      updateNumericInput(session, "selectMtry", value = round(sqrt(ncol(data)), 0))
    }
  })
  observe({
    if(!is.factor(data[, input$SelectY])){
      updateNumericInput(session, "mtryMin", value = round(ncol(data)/3,0)-1)
      updateNumericInput(session, "mtryMax", value = round(ncol(data)/3,0)+1)
      updateNumericInput(session, "mtryBy", value = 1)
    }else{
      updateNumericInput(session, "mtryMin", value = round(sqrt(ncol(data)), 0)-1)
      updateNumericInput(session, "mtryMax", value = round(sqrt(ncol(data)), 0)+1)
      updateNumericInput(session, "mtryBy", value = 1)
    }
  })

  #reactive mtry
  mtryReact <- reactive({
    if(!input$mtry){
      mtry <- input$selectMtry
      return(mtry)
    }else{
      mtry <- seq(input$mtryMin, input$mtryMax, by = input$mtryBy)
      return(mtry)
    }
  })

  #maxnodes
  maxnodesReact <- reactive({
    if(!input$maxnodes){
      maxnodes <- input$selectMaxnodes
      return(maxnodes)
    }else{
      maxnodes <- seq(input$maxnodesMin, input$maxnodesMax, by = input$maxnodesBy)
      return(maxnodes)
    }
  })

  #nodesize
  observe({
    if(!is.factor(data[, input$SelectY])){
      updateNumericInput(session, "selectNodesize", value = 5)
    }else{
      updateNumericInput(session, "selectNodesize", value = 1)
    }
  })

  #reactive nodesize
  nodesizeReact <- reactive({
    if(!input$nodesize){
      nodesize <- input$selectNodesize
      return(nodesize)
    }else{
      nodesize <- seq(input$nodesizeMin, input$nodesizeMax, by = input$nodesizeBy)
      return(nodesize)
    }
  })
  #criterion
  observe({
    if(!is.factor(data[, input$SelectY])){
      updatePickerInput(session, "selectCriterion", choices= c("RMSE", "MAPE", "R2"))
    }else{
      if(nlevels(data[, input$SelectY]) > 2){
        updatePickerInput(session, "selectCriterion", choices= "CONF")
      }else{
        updatePickerInput(session, "selectCriterion", choices= c("AUC", "CONF"))
      }
    }
  })

  #Data
  RFGo <- reactive({
    input$GoModel
    isolate({
      y <- data[, input$SelectY]
      cvcol <- cvcol()
      namesX <- names(data)[which(!names(data)%in% c(input$SelectY))]
      x <- data[, namesX]
      ntree <- ntreeReact()
      mtry <- mtryReact()
      criterion <- input$selectCriterion
      #
      if(input$GoModel != 0){
        res <- rfMod(x = x, y = y, cvcol= cvcol, ntree= ntree, mtry = mtry,  criterion = criterion)
        res

      }
    })
  })

  output$boxparam <- renderUI({
    if(is.null(RFGo()))return(NULL)
    resScore <- RFGo()
    resScore <- resScore[(names(resScore)%in%c("param"))]
    resScore <- as.data.frame(resScore)
    resScore <- round(resScore, 3)


    out <- ""
    for(i in 1:length(resScore)){
      out <- paste0(out, exaBox(title = names(resScore)[i], value = resScore[i], icon = "thunParam.png",
                                width = 3))
    }

      HTML(out)
  })

  output$boxscore<- renderUI({
    if(is.null(RFGo()))return(NULL)
    resScore <- RFGo()
    resScore <- resScore[(names(resScore)%in%c("RMSE", "MAPE", "R2", "AUC", "confusion"))]
    resScore <- as.data.frame(resScore)
    resScore <- round(resScore, 3)



    out <- ""
    for(i in 1:length(resScore)){
      out <- paste0(out, exaBox(title = names(resScore)[i], value = resScore[i],
                                icon = ifelse(names(resScore)[i] == RFGo()$criterion, "mainSco.png", "gear.png"),
                                width = 4))
    }

    HTML(out)
  })

  output$graphOP <- renderAmCharts({
    if(is.null(RFGo())){
      return(NULL)
    }
    if(!is.factor(data[, input$SelectY])){
      plot(RFGo(), type = "obsPred", digits = 3, color = "#00b300")
    }
  })

  output$varimp <- renderAmCharts({
    if(is.null(RFGo())){
      return(NULL)
    }
    if(!is.factor(data[, input$SelectY])){
      plot(RFGo(), type = "importance", digits = 3, color = "#00b300")
    }
  })




  # output$CV <- renderPrint({
  #   print(RFGo())
  # })

})#fin shinserver

