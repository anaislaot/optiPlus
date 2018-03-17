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
                                           selectInput("selectDesc2", label = "Select another variable among the list", selected = NULL,
                                                       choices = names(data))
                          )


                          ,width = 3),#fin sidebarpanel

                        # Show a plot of the generated distribution
                        mainPanel(
                          amChartsOutput("hist"), width = 9
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
                                           radioGroupButtons("cvcol",label = "Do Cross validation :",
                                                             choices = c("By kfolds", "Select a column"), individual = TRUE,
                                                             checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                                                           style = "color: steelblue"),
                                                                              no = tags$i(class = "fa fa-circle-o",
                                                                                          style = "color: steelblue"))),
                                           conditionalPanel("input.cvcol == 'By kfolds'",
                                                            numericInput("selectFolds", label = "select Kfold", value = 5,
                                                                         min = 1)
                                           ),
                                           conditionalPanel("input.cvcol == 'Select a column'",
                                                            # selectInput("SelectColumn", label = "Select a column", selected = NULL,
                                                            #             choices = names(data))
                                                            pickerInput("SelectColumn",label = "Select a column",selected = NULL,
                                                                        choices = names(data), options = list(style = "btn-primary"))
                                           ),
                                           h3("Parameters"),
                                           conditionalPanel("input.ntree == false",
                                                            numericInput("selectNtree", label = "Ntree", value = 500, min = 1)
                                           ),
                                           prettyCheckbox(
                                             inputId = "ntree", label = "ntree grid", icon = icon("check"), status = "primary"
                                           ),
                                           conditionalPanel("input.ntree == true",
                                                            dropdownButton(div(
                                                              numericInput("ntreeMin", label = "Min", value = 100),
                                                              numericInput("ntreeMax", label = "Max", value = 500),
                                                              numericInput("ntreeBy", label = "By", value = 100),
                                                              style = "color : black"),
                                                              status = "primary", label = "grid", circle = FALSE)
                                           ),#conditionalPanel Ntreegrid

                                           conditionalPanel("input.mtry == false",
                                                            numericInput("selectMtry", label = "Mtry", value = 2, min = 1)
                                           ),

                                           prettyCheckbox(
                                             inputId = "mtry", label = "mtry grid", icon = icon("check"), status = "primary"
                                           ),
                                           conditionalPanel("input.mtry == true",
                                                            dropdownButton(div(
                                                              numericInput("mtryMin", label = "Min", value = 1),
                                                              numericInput("mtryMax", label = "Max", value = 2),
                                                              numericInput("mtryBy", label = "By", value = 1),
                                                              style = "color : black"),
                                                              status = "primary", label = "grid", circle = FALSE)
                                           ),
                                           conditionalPanel("input.maxnodes == false",
                                                            numericInput("selectMaxnodes", label = "Maxnodes", value = 10, min = 1)
                                           ),
                                           prettyCheckbox(
                                             inputId = "maxnodes", label = "maxnodes grid", icon = icon("check"), status = "primary"
                                           ),
                                           conditionalPanel("input.maxnodes == true",
                                                            dropdownButton(div(
                                                              numericInput("maxnodesMin", label = "Min", value = 1),
                                                              numericInput("maxnodesMax", label = "Max", value = 10),
                                                              numericInput("maxnodesBy", label = "By", value = 2),
                                                              style = "color : black"),
                                                              status = "primary", label = "grid", circle = FALSE)
                                           ),#conditionalPanel maxnodes
                                           conditionalPanel("input.nodesize == false",
                                                            numericInput("selectNodesize", label = "Nodesize", value = 5, min = 1)
                                           ),

                                           prettyCheckbox(
                                             inputId = "nodesize", label = "nodesize grid", icon = icon("check"), status = "primary"
                                           ),
                                           conditionalPanel("input.nodesize == true",
                                                            dropdownButton(div(
                                                              numericInput("nodesizeMin", label = "Min", value = 1),
                                                              numericInput("nodesizeMax", label = "Max", value = 10),
                                                              numericInput("nodesizeBy", label = "By", value = 1),
                                                              style = "color : black"),
                                                              status = "primary", label = "grid", circle = FALSE)
                                           ),
                                           pickerInput(inputId = "selectCriterion",label = "Select criterion",selected = "RMSE",
                                                       choices = c("RMSE", "MAPE", "R2", "AUC", "CONF"), options = list(style = "btn-primary")),
                                           br(), br(),
                                           actionBttn(inputId = "GoModel", label = "Launch", style = "gradient",
                                                      color = "primary", icon = icon("thumbs-up"))


                          )#fin conditionalPanel de la RF

                          ,width =3),#fin sidebarPanel
                        mainPanel(
                          fluidRow(
                            column(8, align="center",
                          dataTableOutput("upload")
                            )
                          ),
                          fluidRow(
                            column(8, align="center",
                                   amChartsOutput("graphOP")
                            )
                          )


                          , width = 9 )

                      )
             ),#fermeture page1
             ####PAGE 3 :
             tabPanel("Maye your predict"



             ),#fermeture page3
             tabPanel("Balance sheet"



             )#fermeture page4
  )#fermeture navbarpage
))#fermeture fluidpage + shinyUI
