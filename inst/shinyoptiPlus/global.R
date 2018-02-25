library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rAmCharts)
library(ggplot2)

data <- diamonds
data <- as.data.frame(diamonds)
data <- data[1:100, ]


# data <- mtcars

# data <- iris

# data <- read.csv("C:/Anais/Projets stat/mycotox/Code/Data/base_FUMO.csv", sep =";", dec = ".", header = TRUE)
# data$FUMO <- as.factor(data$FUMO)
