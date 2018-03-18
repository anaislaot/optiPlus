#metrice confusion ramcharts

.colorData <- function(data, nbclasses=NULL, col=c("#FF0000","#FFFFFF","#0000FF"), colorby="all")
{

  if(colorby=="all")
  {
    framclasses <- matrix(0,nrow=nrow(data),ncol=ncol(data))
    values <- unlist(c(data))
    if(nbclasses < length(unique(values))){
      classes <- quantile(values,seq(from = 0, to = 1,length.out = nbclasses+1))
      for(i in 1:(length(classes)-1))
      {
        framclasses=framclasses+((data>=classes[i])+1-1)
      }
    }else{
      nbclasses <- length(unique(values))
      classes <- sort(unique(values))
      for(i in 1:length(classes))
      {
        framclasses=framclasses+((data>=classes[i])+1-1)
      }
    }
  }

  if(colorby=="col")
  {
    framclasses <- matrix(0,nrow=nrow(data),ncol=ncol(data))
    for(j in 1:ncol(data))
    {
      classes <- quantile(sort((unlist(c(data[,j])))),seq(from = 0, to = 1,length.out = nbclasses+1))

      for(i in 1:(length(classes)-1))
      {
        framclasses[,j]=framclasses[,j]+((data[,j]>=classes[i])+1-1)
      }
    }
  }

  if(colorby=="row")
  {
    framclasses <- matrix(0,nrow=nrow(data),ncol=ncol(data))
    for(j in 1:nrow(data))
    {

      classes <- quantile(sort((unlist(c(data[j,])))),seq(from = 0, to = 1,length.out = nbclasses+1))
      for(i in 1:(length(classes)-1))
      {
        framclasses[j,]=framclasses[j,]+((data[j,]>=classes[i])+1-1)
      }
    }
  }

  color <- colorRampPalette(col)(nbclasses)
  for(i in 1:length(color)){
    framclasses[framclasses==as.character(i)] <- color[i]
  }
  framclasses <- data.frame(framclasses)
  names(framclasses) <- paste0(names(data),"col")
  framclasses[] <- lapply(framclasses, as.character)
  list(data = cbind(data,framclasses), classes = list(nclasses = nbclasses, labels = classes))
}

#' Associeted constructor data.frame to initial data.frame
#' @param data : data.frame
#'
#' @return data.frame compound to original data.frame and associated constructor data.frame
.constructdata <- function(data){
  construct <- matrix(1,ncol=ncol(data)/2,nrow=nrow(data))
  construct <- data.frame(construct)
  names(construct) <- paste0(names(data)[1:(ncol(data)/2)],"construct")
  return(cbind(row=row.names(data),data,construct))
}


#' Make chart
#' @param data : data.frame
#' @param labels : TRUE FALSE, display labels
#' @param cex : size of labels
#' @param xLabelsRotation : rotation of xlabels
#' @param colorby : can be "all","row","col".
#' @param col : 3 col to use in colorRampPalette
#' @param nbclasses : number of classes
#'
#'
#' @return data.frame compound to original data.frame and associated constructor data.frame
.heatmap <- function(data, classes, labels = TRUE, cex=10, main="", xLabelsRotation=45,
                    colorby="all", col=c("#FF0000","#FFFFFF","#0000FF"), tooltipLabel = "count"){

  ncate <-(ncol(data)-1)/3

  namecat <- names(data[,2:(ncate+1)])

  values <- paste0("['", paste(namecat, collapse = "','"), "']")

  chart <- sapply(namecat, function(x) {
    amGraph(balloonText=paste0("<b>[[title]]-[[category]]</b><br><b> ", tooltipLabel, " : </b>[[",x,"]]"),
            fillAlphas=0.8,labelText=if(labels){paste0("[[",x,"]]")}else{""},lineAlpha=0.3,fontSize=cex,
            title=x,type="column",fillColorsField=paste0(x,"col"),valueField=paste0(x,"construct"))},USE.NAMES = FALSE
  )

  guides = list()
  n <- length(colnames(data[,2:(ncate+1)]))
  k <- 0
  for(i in 1:n){
    k <- k +1
    guides[[k]] <- guide(id=paste0("guide",i),value=i,toValue=i,lineAlpha=1,color="#000000",lineThickness=1)

  }

  n <- nrow(data)
  for(i in 1:n){
    guides[[k]] = guide(id=paste0("guide",k),category=row.names(data)[i],lineAlpha=1,color="#000000",lineThickness=1,above=TRUE,expand=TRUE)
    k <- k +1
  }

  legendlist <- list()

  if(colorby=="all"){

    nbclasses <- classes$nclasses
    classes <- classes$labels
    color <- colorRampPalette(col)(nbclasses)

    associated <- NULL
    if(nbclasses < length(classes)){
      for(i in 1:length(classes)-1){
        associated[i] <- paste0("[",classes[i]," , ",classes[i+1], ifelse(i==length(classes)-1, "]", "["))
      }
    }else{
      associated <- classes
    }

    datatemp <- data.frame(title=associated,color=color)
    for(i in 1:nrow(datatemp))
    {
      legendlist[[i]] <- list(title=as.character(datatemp[i,1]),color = as.character(datatemp[i,2]))
    }
  }else{
    legendlist[[1]]<-list(title="Low",color = as.character(col)[1])
    legendlist[[2]]<-list(title="Medium",color = as.character(col)[2])
    legendlist[[3]]<-list(title="Large",color = as.character(col)[3])
  }

  amSerialChart(categoryField="row")%>>%
    setBalloon(borderThickness = 0) %>>%
    setDataProvider(data) %>>%
    setProperties(columnWidth = 1,
                  gridAboveGraphs=TRUE,rotate=TRUE)%>>%
    setGuides(guides)%>>%
    addTitle(text = main)%>>%
    setLegend(data=(legendlist), markerBorderColor="#000000", align = "center", position = "right")%>>%
    addValueAxes(stackType="regular",axisAlpha=0,gridThickness=0,gridAlpha=1,position="left",labelRotation=xLabelsRotation,maximum=ncate,
                 labelFunction = htmlwidgets::JS(paste0("function(value,valueString,axis){
                                                        Math.trunc = Math.trunc || function(x) {
                                                        return x < 0 ? Math.ceil(x) : Math.floor(x);
                                                        };
                                                        var val = ", values, ";
                                                        var indice = Math.trunc(value);
                                                        if(indice < val.length && value % 1 != 0){
                                                        return val[indice];
                                                        }else{
                                                        return '';
                                                        }
                                                        ;}")))%>>%
    setGraphs(chart)%>>%
    setCategoryAxis(gridPosition="start",axisAlpha=1,gridThickness=0,gridAlpha=1)%>>%
    setExport(enabled = TRUE,
              menu = list(
                list(
                  class = "export-main",
                  menu = list(
                    list(
                      label = "Download as ...",
                      menu = list("PNG", "JPG", "SVG", "PDF")
                    ),
                    list(
                      label = "Save data as CSV",
                      click = htmlwidgets::JS(paste0('function() {
                                                     var cfg = {
                                                     data: this.getChartData(),
                                                     delimiter: ",",
                                                     quotes: true,
                                                     escape: true,
                                                     dateFields: [],
                                                     dateFormat: this.setup.chart.dataDateFormat || "YYYY-MM-DD"
                                                     };
                                                     var data = "";

                                                     if ( this.setup.chart.categoryAxis && this.setup.chart.categoryAxis.parseDates && this.setup.chart.categoryField ) {
                                                     cfg.dateFields.push( this.setup.chart.categoryField );
                                                     }

                                                     //header
                                                     row = 0;
                                                     var buffer = [];
                                                     var cpt = 1;
                                                     for ( col in cfg.data[ row ] ) {
                                                     if(cpt <= ', ((ncol(data)-1)/3)+1, '){
                                                     var value = cfg.data[ row ][col];
                                                     value = col;

                                                     if ( typeof value === "string" ) {
                                                     if ( cfg.escape ) {
                                                     value = value.replace( \'"\', \'""\' );
                                                     }
                                                     if ( cfg.quotes ) {
                                                     value = [ \'"\', value, \'"\' ].join( "" );
                                                     }
                                                     }

                                                     buffer.push( value );
                                                     cpt = cpt+1;
                                                     }
                                                     }
                                                     data += buffer.join( cfg.delimiter ) + "\\n";

                                                     for ( row in cfg.data ) {
                                                     var cpt = 1;
                                                     var buffer = [];

                                                     for ( col in cfg.data[ row ] ) {
                                                     if(cpt <= ', ((ncol(data)-1)/3)+1, '){
                                                     var value = cfg.data[ row ][ col ];

                                                     if ( typeof value === "string" ) {
                                                     value = value;
                                                     } else if ( cfg.dateFormat && value instanceof Date && cfg.dateFields.indexOf( col ) != -1 ) {
                                                     value = AmCharts.formatDate( value, cfg.dateFormat );
                                                     }

                                                     // WRAP IN QUOTES
                                                     if ( typeof value === "string" ) {
                                                     if ( cfg.escape ) {
                                                     value = value.replace( \'"\', \'""\' );
                                                     }
                                                     if ( cfg.quotes ) {
                                                     value = [ \'"\', value, \'"\' ].join( "" );
                                                     }
                                                     }

                                                     buffer.push( value );
                                                     cpt = cpt +1;
                                                     }
                                                     }
                                                     data += buffer.join( cfg.delimiter ) + "\\n";
                                                     };
                                                     this.download( data, "text/plain", "heatmap.csv" );}')
                                                     ))
                                                     )
                                                     )
                                                     )

                                                     )

                                                     }


#' Amchart Heat-Map
#' @param data : data.frame, should be a contingency table
#' @param nbclasses : number of classes
#' @param col : 3 col to use in colorRampPalette
#' @param labels : TRUE FALSE, display labels
#' @param cex : size of labels
#' @param main : title
#' @param xLabelsRotation : rotation of xlabels
#' @param colorby : can be "all","row","col".
#' @param legend : TRUE or FALSE, display legend
#'
#' @examples
#'
# data(USArrests, "VADeaths")
# USArrests <- USArrests [1:10,]
#
# .amHeatmap(USArrests)

# amHeatmap(USArrests, xLabelsRotation = 0, tooltipLabel = "mape *") %>%
#     amOptions(creditsPosition = "top-right", main = "Titre")

#' amHeatmap(USArrests, nclasses=5, col=c("#FF0000","#FFFFFF","#0000FF"),
#'     labels = TRUE, cex=10, main="My title", xLabelsRotation=45, colorby="all",legend = TRUE)
#'
#' amHeatmap(USArrests, nclasses=5, col=c("#FF0000","#FFFFFF","#0000FF"), labels = TRUE, cex=10,
#'     main="My title", xLabelsRotation=45, colorby="row",l egend = TRUE)
#'
#' amHeatmap(USArrests, nclasses=5, col=c("#FF0000","#FFFFFF","#0000FF"),labels = TRUE, cex=10,
#'     main="My title", xLabelsRotation=45, colorby="col", legend = TRUE)
#'
#' amHeatmap(USArrests, nclasses=10, col=c("#00FF00","#FF00FF","#0000FF"),labels = TRUE, cex=10,
#'     main="My title", xLabelsRotation=45, colorby="all",legend = TRUE)
#'
#' @return data.frame compound to original data.frame and associated constructor data.frame
#'
.amHeatmap <- function(data, nclasses = 5, col = c("#FF0000","#FFFFFF","#0000FF"),
                      labels = TRUE, cex = 10, main = "", xLabelsRotation = 45,
                      colorby = "all", legend = TRUE, tooltipLabel = "count"){
  colordata <- .colorData(data, nclasses, col, colorby)
  data <- .constructdata(colordata$data)
  .heatmap(data, colordata$classes, labels, cex, main, xLabelsRotation, colorby, col, tooltipLabel)
}

# data <- USArrests
#
# data <- data.frame(a = c(3,0), b = c(2,1))
# .amHeatmap(data)
#
# nclasses = 5
# col = c("#FF0000","#FFFFFF","#0000FF")
# colorby="all"

# toCharData <- function(data){
#
#   res <- paste0(paste("'row'", paste0("'", paste(colnames(data), collapse = "','"), "'"), sep = ","), "\\n")
#
#   ctrl <- sapply(1:nrow(data), function(x){
#     ligne <- paste0(paste(paste0("'", rownames(data)[x], "'"), paste0("'",paste(data[x, ], collapse = "','"), "'"), sep = ","), ifelse(x==nrow(data), "", "\\n"))
#     res <<- paste0(res, ligne)
#     NULL
#   })
#
#   res
# }


#density ggplot2
.ggplot2.density <- function (data, xName = NULL, groupName = NULL, addMeanLine = FALSE,
                              meanLineColor = NULL, meanLineType = "dashed", meanLineSize = 1,
                              densityFill = NULL, fillGroupDensity = FALSE, colorGroupDensityLine = FALSE,
                              groupColors = NULL, brewerPalette = NULL, ...)
{
  ggplot2.setAxis <- function (plot, xShowTitle = TRUE, yShowTitle = TRUE, xtitle = NULL,
                               ytitle = NULL, xtitleFont = c(14, "bold", "black"), ytitleFont = c(14,
                                                                                                  "bold", "black"), xlim = NULL, ylim = NULL, xScale = c("none",
                                                                                                                                                         "log2", "log10"), yScale = c("none", "log2", "log10"),
                               xShowTickLabel = TRUE, yShowTickLabel = TRUE, xTickLabelFont = c(12,
                                                                                                "bold", "black"), yTickLabelFont = c(12, "bold", "black"),
                               xtickLabelRotation = 0, ytickLabelRotation = 0, hideAxisTicks = FALSE,
                               axisLine = c(0.5, "solid", "#E5E5E5"))
  {
    if (!is.null(xtitle))
      plot <- plot + xlab(xtitle)
    if (!is.null(ytitle))
      plot <- plot + ylab(ytitle)
    if (!xShowTitle)
      plot <- plot + theme(axis.title.x = element_blank())
    else plot <- plot + theme(axis.title.x = element_text(size = as.numeric(xtitleFont[1]),
                                                          face = xtitleFont[2], colour = xtitleFont[3], hjust = 0.4))
    if (!yShowTitle)
      plot <- plot + theme(axis.title.y = element_blank())
    else plot <- plot + theme(axis.title.y = element_text(size = as.numeric(ytitleFont[1]),
                                                          face = ytitleFont[2], colour = ytitleFont[3], vjust = 0.4,
                                                          angle = 90))
    if (!is.null(ylim))
      plot <- plot + ylim(ylim[1], ylim[2])
    if (!is.null(xlim))
      plot <- plot + xlim(xlim[1], xlim[2])
    if (xScale[1] == "log2")
      plot <- plot + scale_x_continuous(trans = "log2")
    else if (xScale[1] == "log10")
      plot <- plot + scale_x_log10()
    if (yScale[1] == "log2")
      plot <- plot + scale_y_continuous(trans = "log2")
    else if (yScale[1] == "log10")
      plot <- plot + scale_y_log10()
    if (!xShowTickLabel)
      plot <- plot + theme(axis.text.x = element_blank())
    else plot <- plot + theme(axis.text.x = element_text(size = as.numeric(xTickLabelFont[1]),
                                                         face = xTickLabelFont[2], colour = xTickLabelFont[3],
                                                         angle = xtickLabelRotation))
    if (!yShowTickLabel)
      plot <- plot + theme(axis.text.y = element_blank())
    else plot <- plot + theme(axis.text.y = element_text(size = as.numeric(yTickLabelFont[1]),
                                                         face = yTickLabelFont[2], colour = yTickLabelFont[3],
                                                         angle = ytickLabelRotation))
    if (hideAxisTicks)
      plot <- plot + theme(axis.ticks = element_blank())
    plot <- plot + theme(axis.line = element_line(size = as.numeric(axisLine[1]),
                                                  linetype = axisLine[2], colour = axisLine[3]))
    plot
  }
  ggplot2.customize <- function (plot, ...)
  {
    args = list(...)
    if (!is.null(args$faceting))
      faceting = args$faceting
    else faceting = FALSE
    if (!is.null(args$facetingVarNames))
      facetingVarNames = args$facetingVarNames
    else facetingVarNames = NULL
    if (!is.null(args$facetingDirection))
      facetingDirection = args$facetingDirection
    else facetingDirection = "vertical"
    if (!is.null(args$facetingScales))
      facetingScales = args$facetingScales
    else facetingScales = "fixed"
    if (!is.null(args$facetingFont))
      facetingFont = args$facetingFont
    else facetingFont = c(size = 12, font = "plain", color = "black")
    if (!is.null(args$facetingRect))
      facetingRect = args$facetingRect
    else facetingRect = list(background = NULL, lineType = NULL,
                             lineColor = NULL, lineSize = NULL)
    if (!is.null(args$facetingTextAngles))
      facetingTextAngles = args$facetingTextAngles
    else facetingTextAngles = c(NULL, NULL)
    if (faceting && !is.null(facetingVarNames)) {
      if (length(facetingVarNames) == 1) {
        if (facetingDirection == "vertical")
          plot <- plot + facet_grid(as.formula(paste(facetingVarNames,
                                                     " ~ .", sep = "")), scales = facetingScales)
        else if (facetingDirection == "horizontal")
          plot <- plot + facet_grid(as.formula(paste(". ~ ",
                                                     facetingVarNames, sep = "")), scales = facetingScales)
      }
      else if (length(facetingVarNames) == 2)
        plot <- plot + facet_grid(as.formula(paste(facetingVarNames[1],
                                                   "~ ", facetingVarNames[2], sep = "")), scales = facetingScales)
    }
    if (!is.null(args$backgroundColor)) {
      backgroundColor <- args$backgroundColor
      if (backgroundColor %in% c("gray", "grey"))
        plot <- plot + theme_gray()
      else if (backgroundColor == "white")
        plot <- plot + theme_bw()
      else {
        plot <- plot + theme(panel.background = element_rect(fill = backgroundColor,
                                                             size = 0.5, linetype = "solid", colour = backgroundColor))
      }
    }
    if (faceting && !is.null(facetingVarNames)) {
      plot <- plot + theme(strip.text.x = element_text(size = facetingFont[1],
                                                       face = facetingFont[2], color = facetingFont[3],
                                                       angle = facetingTextAngles[1]), strip.text.y = element_text(size = facetingFont[1],
                                                                                                                   face = facetingFont[2], color = facetingFont[3],
                                                                                                                   angle = facetingTextAngles[2]), strip.background = element_rect(fill = facetingRect$background,
                                                                                                                                                                                   colour = facetingRect$lineColor, linetype = facetingRect$lineType,
                                                                                                                                                                                   size = facetingRect$lineSize))
    }
    if (!is.null(args$xShowTitle))
      xShowTitle <- args$xShowTitle
    else xShowTitle = TRUE
    if (!is.null(args$yShowTitle))
      yShowTitle <- args$yShowTitle
    else yShowTitle = TRUE
    if (!is.null(args$xtitle))
      xtitle <- args$xtitle
    else xtitle = NULL
    if (!is.null(args$ytitle))
      ytitle <- args$ytitle
    else ytitle = NULL
    if (!is.null(args$xtitleFont))
      xtitleFont <- args$xtitleFont
    else xtitleFont = c(14, "bold", "black")
    if (!is.null(args$ytitleFont))
      ytitleFont <- args$ytitleFont
    else ytitleFont = c(14, "bold", "black")
    if (!is.null(args$ylim))
      ylim <- args$ylim
    else ylim = NULL
    if (!is.null(args$xlim))
      xlim <- args$xlim
    else xlim = NULL
    if (!is.null(args$xScale))
      xScale <- args$xScale
    else xScale = c("none", "log2", "log10")
    if (!is.null(args$yScale))
      yScale <- args$yScale
    else yScale = c("none", "log2", "log10")
    if (!is.null(args$xShowTickLabel))
      xShowTickLabel <- args$xShowTickLabel
    else xShowTickLabel = TRUE
    if (!is.null(args$yShowTickLabel))
      yShowTickLabel <- args$yShowTickLabel
    else yShowTickLabel = TRUE
    if (!is.null(args$xTickLabelFont))
      xTickLabelFont <- args$xTickLabelFont
    else xTickLabelFont = c(12, "bold", "black")
    if (!is.null(args$yTickLabelFont))
      yTickLabelFont <- args$yTickLabelFont
    else yTickLabelFont = c(12, "bold", "black")
    if (!is.null(args$xtickLabelRotation))
      xtickLabelRotation <- args$xtickLabelRotation
    else xtickLabelRotation = 0
    if (!is.null(args$ytickLabelRotation))
      ytickLabelRotation <- args$ytickLabelRotation
    else ytickLabelRotation = 0
    if (!is.null(args$hideAxisTicks))
      hideAxisTicks <- args$hideAxisTicks
    else hideAxisTicks = FALSE
    if (!is.null(args$axisLine))
      axisLine <- args$axisLine
    else axisLine = c(0.5, "solid", "#E5E5E5")
    plot <- ggplot2.setAxis(plot, xShowTitle = xShowTitle, yShowTitle = yShowTitle,
                            xtitle = xtitle, ytitle = ytitle, xtitleFont = xtitleFont,
                            ytitleFont = ytitleFont, xlim = xlim, ylim = ylim, xScale = xScale,
                            yScale = yScale, xShowTickLabel = xShowTickLabel, yShowTickLabel = yShowTickLabel,
                            xTickLabelFont = xTickLabelFont, yTickLabelFont = yTickLabelFont,
                            xtickLabelRotation = xtickLabelRotation, ytickLabelRotation = ytickLabelRotation,
                            hideAxisTicks = hideAxisTicks, axisLine = axisLine)
    if (!is.null(args$mainTitle))
      plot <- plot + ggtitle(args$mainTitle)
    if (!is.null(args$mainTitleFont))
      mainTitleFont = args$mainTitleFont
    else mainTitleFont = c(14, "bold", "black")
    plot <- plot + theme(plot.title = element_text(size = as.numeric(mainTitleFont[1]),
                                                   lineheight = 1, face = mainTitleFont[2], colour = mainTitleFont[3]))
    if (!is.null(args$gridColor)) {
      gridColor = args$gridColor
      plot <- plot + theme(panel.grid.major = element_line(size = 0.5,
                                                           linetype = "solid", colour = gridColor)) + theme(panel.grid.minor = element_line(size = 0.25,
                                                                                                                                            linetype = "solid", colour = gridColor))
    }
    if (!is.null(args$removePanelBorder)) {
      removePanelBorder = args$removePanelBorder
      if (removePanelBorder)
        plot <- plot + theme(panel.border = element_blank())
    }
    if (!is.null(args$removePanelGrid)) {
      removePanelGrid = args$removePanelGrid
      if (removePanelGrid)
        plot <- plot + theme(panel.grid.minor = element_blank(),
                             panel.grid.major = element_blank(), axis.line = element_line(size = as.numeric(axisLine[1]),
                                                                                          linetype = axisLine[2], colour = axisLine[3]))
    }
    if (!is.null(args$showLegend))
      showLegend = args$showLegend
    else showLegend = TRUE
    if (!is.null(args$legendPosition))
      legendPosition = args$legendPosition
    else legendPosition = "right"
    if (showLegend == TRUE)
      plot <- plot + theme(legend.position = legendPosition)
    else plot <- plot + theme(legend.position = "none")
    if (showLegend == TRUE) {
      if (!is.null(args$legendBackground))
        legendBackground = args$legendBackground
      else legendBackground = c("#FFFFFF", 0.5, "blank", "black")
      if (!is.null(args$legendTextFont))
        legendTextFont = args$legendTextFont
      else legendTextFont = c(10, "plain", "black")
      if (!is.null(args$legendTitleFont))
        legendTitleFont = args$legendTitleFont
      else legendTitleFont = c(10, "bold", "black")
      plot <- plot + theme(legend.title = element_text(size = as.numeric(legendTitleFont[1]),
                                                       face = legendTitleFont[2], colour = legendTitleFont[3])) +
        theme(legend.text = element_text(size = as.numeric(legendTextFont[1]),
                                         face = legendTextFont[2], colour = legendTextFont[3])) +
        theme(legend.background = element_rect(fill = legendBackground[1],
                                               size = as.numeric(legendBackground[2]), linetype = legendBackground[3],
                                               colour = legendBackground[4]))
      if (!is.null(args$legendTitle))
        plot <- plot + labs(fill = args$legendTitle, colour = args$legendTitle,
                            shape = args$legendTitle)
      if (!is.null(args$legendItemOrder))
        plot <- plot + scale_x_discrete(limits = as.character(args$legendItemOrder))
    }
    if (!is.null(args$orientation))
      orientation = args$orientation
    else orientation = "standard"
    if (orientation == "horizontal")
      plot <- plot + coord_flip()
    if (orientation == "yAxisReversed")
      plot <- plot + scale_y_reverse()
    plot
  }


  .standard_params <- function (...)
  {
    x <- list(...)
    res <- list()
    res$color <- ifelse(!is.null(x$color), x$color, "black")
    res$color <- ifelse(!is.null(x$colour), x$colour, res$color)
    res$linetype <- ifelse(!is.null(x$linetype), x$linetype,
                           "solid")
    res$size <- ifelse(!is.null(x$size), x$size, 1)
    res$fill <- ifelse(!is.null(x$fill), x$fill, "black")
    res$shape <- ifelse(!is.null(x$shape), x$shape, 19)
    res
  }
  spms <- .standard_params(...)
  if (is.null(xName) & !is.numeric(data))
    stop("xName is missing or NULL. In this case data should be a numeric vector")
  else if (is.numeric(data)) {
    data = cbind(x = data, grp = rep(1, length(data)))
    xName = "x"
  }
  data = data.frame(data)
  if (is.null(groupName)) {
    p <- ggplot(data = data, aes_string(x = xName))
    if (!is.null(densityFill) && densityFill != "")
      p <- p + geom_density(fill = densityFill, linetype = spms$linetype,
                            colour = spms$color)
    else p <- p + geom_density(linetype = spms$linetype,
                               colour = spms$color)
  }
  else {
    data[, groupName] = factor(data[, groupName])
    if (fillGroupDensity == TRUE) {
      if (colorGroupDensityLine)
        p <- ggplot(data = data, aes_string(x = xName,
                                            fill = groupName, colour = groupName))
      else p <- ggplot(data = data, aes_string(x = xName,
                                               fill = groupName))
    }
    else p <- ggplot(data = data, aes_string(x = xName,
                                             colour = groupName))
    p <- p + geom_density(alpha = 0.5)
  }
  if (addMeanLine) {
    if (is.null(groupName)) {
      if (is.null(meanLineColor))
        meanLineColor = "red"
      m = mean(data[, xName], na.rm = T)
      p <- p + geom_vline(aes_string(xintercept = m),
                          color = meanLineColor, linetype = meanLineType,
                          size = meanLineSize)
    }
    else {
      df <- data.frame(grp = factor(data[, groupName]),
                       x = data[, xName])
      df.m <- stats::aggregate(df[, "x"], by = list(grp = df[,
                                                             "grp"]), mean)
      names(df.m) <- c(groupName, "x.mean")
      if (is.null(meanLineColor))
        p <- p + geom_vline(data = df.m, aes_string(xintercept = "x.mean",
                                                    colour = groupName), linetype = meanLineType,
                            size = meanLineSize)
      else p <- p + geom_vline(data = df.m, aes_string(xintercept = "x.mean",
                                                       colour = groupName), linetype = meanLineType,
                               color = meanLineColor, size = meanLineSize)
    }
  }
  if (!is.null(groupColors)) {
    p <- p + scale_fill_manual(values = groupColors)
    p <- p + scale_colour_manual(values = groupColors)
  }
  else if (!is.null(brewerPalette)) {
    p <- p + scale_fill_brewer(palette = brewerPalette)
    p <- p + scale_colour_brewer(palette = brewerPalette,
                                 guide = "none")
  }
  p <- ggplot2.customize(p, ...)
  p
}
