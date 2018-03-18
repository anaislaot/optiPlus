#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyUI(fluidPage(
  #tags$head(tags$style(HTML("dashboardStyle.css"))),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "exabox.css")
  ),
  div(class = "footeropti"),
  navbarPage("optiPlus",

             ####PAGE 1 :

             tabPanel("Data",

                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(
                          # selectInput("selectDesc", label = "Select among the list", selected = NULL,
                          #             choices = names(data)),
                          pickerInput("selectDesc",label = "Select among the list",selected = NULL,
                                      choices = names(data), options = list(style = "btn-primary")),
                          prettyCheckbox(inputId = "var2check", label = "Select another variable",
                                         icon = icon("check"),status = "primary"),
                          conditionalPanel("input.var2check == true",
                                           pickerInput("selectDesc2", label = "Select another variable among the list", selected = NULL,
                                                       choices = names(data), options = list(style = "btn-primary"))

                          )


                          ,width = 3),#fin sidebarpanel

                        # Show a plot of the generated distribution
                        mainPanel(
                          amChartsOutput("hist")
                          , width = 9
                        )
                      )#fin sidebar layout

             ),#fermeture page1

             ####PAGE 2 :
             tabPanel("Let's go to the Models",
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput("selectMethod",label = "Select a method among the list",selected = "Random Forest",
                                      choices = c("Random Forest", "Gradient Boosting", "Logistic Regression", "Lasso"),
                                      options = list(style = "btn-primary")),
                          conditionalPanel("input.selectMethod == 'Random Forest'",
                                           pickerInput("SelectY",label = "Select y",selected = NULL,
                                                       choices = names(data), options = list(style = "btn-primary")),
                                           radioGroupButtons("cvcol",label = "Do cross validation :",
                                                             choices = c("By kfolds", "Select a column"), individual = TRUE,
                                                             checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                                                           style = "color: steelblue"),
                                                                              no = tags$i(class = "fa fa-circle-o",
                                                                                          style = "color: steelblue"))),
                                           conditionalPanel("input.cvcol == 'By kfolds'",
                                                            numericInput("selectFolds", label = "Select kfold", value = 5,
                                                                         min = 1)
                                           ),

                                           conditionalPanel("input.cvcol == 'Select a column'",
                                                            # selectInput("SelectColumn", label = "Select a column", selected = NULL,
                                                            #             choices = names(data))
                                                            pickerInput("SelectColumn",label = "Select a column",selected = NULL,
                                                                        choices = names(data), options = list(style = "btn-primary"))
                                           ),
                                           pickerInput(inputId = "selectCriterion",label = "Select criterion",selected = "RMSE",
                                                       choices = c("RMSE", "MAPE", "R2", "AUC", "CONF"), options = list(style = "btn-primary")),
                                           div(h3("Parameters", class='titlebox'),
                                               br(),
                                               div(h4("Number of trees : ", class='titlebox'),


                                               conditionalPanel("input.ntree == false",
                                                                numericInput("selectNtree", label = "", value = 500, min = 1)
                                               ),
                                            column(5, prettyToggle(
                                                 inputId = "ntree",
                                                 label_on = "ntree grid",
                                                 label_off = "ntree grid",
                                                 icon_on = icon("thumbs-up"),
                                                 icon_off = icon("thumbs-down"),
                                                 status_on = "info",
                                                 status_off = "danger"
                                               )),
                                           column(7,
                                           conditionalPanel("input.ntree == true",
                                                            dropdownButton(div(
                                                              numericInput("ntreeMin", label = "Min", value = 100),
                                                              numericInput("ntreeMax", label = "Max", value = 500),
                                                              numericInput("ntreeBy", label = "By", value = 100),
                                                              style = "color : black"),
                                                              status = "primary", label = "Defined ntree grid", circle = FALSE)
                                           )),#conditionalPanel Ntreegrid
                                            br(), br(),

                                              class = "boerderTB1PX" ),


                                           br(),
                                           div(h4("mtry definition : ", class='titlebox'),

                                           conditionalPanel("input.mtry == false",
                                                            numericInput("selectMtry", label = "", value = 2, min = 1)
                                           ),

                                           column(5,prettyToggle(
                                             inputId = "mtry",
                                             label_on = "mtry grid",
                                             label_off = "mtry grid",

                                             icon_on = icon("thumbs-up"),
                                             icon_off = icon("thumbs-down"),
                                             status_on = "info",
                                             status_off = "danger"
                                           )),
                                           column(7, conditionalPanel("input.mtry == true",
                                                            dropdownButton(div(
                                                              numericInput("mtryMin", label = "Min", value = 1),
                                                              numericInput("mtryMax", label = "Max", value = 2),
                                                              numericInput("mtryBy", label = "By", value = 1),
                                                              style = "color : black"),
                                                              status = "primary", label = "Defined mtry grid", circle = FALSE)
                                           )),
                                           br(), br(),

                                           class = "boerderB1PX" ),

                                           br(),
                                           div(h4("maxnodes definition : ", class='titlebox'),


                                           conditionalPanel("input.maxnodes == false",
                                                            numericInput("selectMaxnodes", label = "", value = 10, min = 1)
                                           ),
                                           column(5,prettyToggle(
                                             inputId = "maxnodes",
                                             label_on = "maxnodes grid",
                                             label_off = "maxnodes grid",
                                             icon_on = icon("thumbs-up"),
                                             icon_off = icon("thumbs-down"),
                                             status_on = "info",
                                             status_off = "danger"
                                           )),
                                           column(7,conditionalPanel("input.maxnodes == true",
                                                            dropdownButton(div(
                                                              numericInput("maxnodesMin", label = "Min", value = 1),
                                                              numericInput("maxnodesMax", label = "Max", value = 10),
                                                              numericInput("maxnodesBy", label = "By", value = 2),
                                                              style = "color : black"),
                                                              status = "primary", label = "Defined maxnodes grid", circle = FALSE)
                                           )),#conditionalPanel maxnodes


                                           br(), br(),

                                           class = "boerderB1PX" ),
                                           br(),
                                           div(h4("Nodesize definition: ", class='titlebox'),

                                           conditionalPanel("input.nodesize == false",
                                                            numericInput("selectNodesize", label = "", value = 5, min = 1)
                                           ),

                                           column(5,prettyToggle(
                                             inputId = "nodesize",
                                             label_on = "nodesize grid",
                                             label_off = "nodesize grid",
                                             icon_on = icon("thumbs-up"),
                                             icon_off = icon("thumbs-down"),
                                             status_on = "info",
                                             status_off = "danger"
                                           )),
                                           column(7,conditionalPanel("input.nodesize == true",
                                                            dropdownButton(div(
                                                              numericInput("nodesizeMin", label = "Min", value = 1),
                                                              numericInput("nodesizeMax", label = "Max", value = 10),
                                                              numericInput("nodesizeBy", label = "By", value = 1),
                                                              style = "color : black"),
                                                              status = "primary", label = "Defined nodesize grid", circle = FALSE)
                                           )),
                                           br(), br()),
                                           br(),class = "divforparam")#fermeture div
                                           ,


                                           br(), br(),
                                           div(
                                           actionBttn(inputId = "GoModel", label = "Launch", style = "gradient",
                                                      color = "primary", icon = icon("thumbs-up"))
                                           , class= "aligndiv")


                          )#fin conditionalPanel de la RF

                          ,width =3),#fin sidebarPanel
                        mainPanel(
                          fluidRow(

                            column(7, align="center",
                                   fluidRow(
                                   div(
                                     fluidRow(
                                   h3("Parameters ", class='titlebox'),
                                   uiOutput("boxparam")
                                     )
                                   , class = 'myParamDiv')
                                   )
                            ),


                            column(5, align="center",
                                   fluidRow(
                                     div(
                                       fluidRow(
                                   h3("Scores", class='titlebox'),
                                   uiOutput("boxscore")
                                       )
                                   , class = 'myParamDiv2')
                                   )

                          ), width = 9),
                          br(), br(),br(), br(),
                         uiOutput("graphModel")

                        )
                      )

                      ),#fermeture page1
                      ####PAGE 3 :
                      tabPanel("Maye your predict"



                      ),#fermeture page3
                      tabPanel("Balance sheet"



                      )#fermeture page4
             , footer = br())#fermeture navbarpage
  ))#fermeture fluidpage + shinyUI
