library(shiny)

shinyUI(fluidPage(
    
    tags$head(tags$style("#wordLinks{color: orange;
                                 font-size: 30px;
                                font-style: bold;}"
    )
    ),
    
    headerPanel("Natural language prediction tool"),
    p("Application was built during Coursera capstone project. It tries to predict user input from enrering (or not) text"),
    fluidRow(
        column( 6,
                numericInput("show_last", 
                             "Predict word count:", 
                             value = 3, 
                             min = 1, 
                             max = 10, 
                             width = "150px")
        )
    ),
    fluidRow(
        column( 6,
                textInput("phrase", 
                          "type your text:", width = "100%")
        )
    ),
    fluidRow(
        column( 2, 
                HTML("<font color=red size=6>prediction is: </font>")
        ),
        column( 5, 
                uiOutput("wordLinks")
        )
    ),
    fluidRow(
        column( 1, 
                checkboxInput("isAdvanced", "Advanced")
        )
    ),
    
    fluidRow(
        column(6,
               hr(), 
        # dataTableOutput("details")
        # uiOutput("advanced")
        conditionalPanel("input.isAdvanced == true", 

                                dataTableOutput("details"))
                         # ,
#                          column(6, 
#                                 # conditionalPanel("output.predict_nrows",
#                                     p("Advanced mode. In table below you can find additional information about prediction result. ")
#                                 # )
#                          )
                                
        )
    )
    #     hr(),
    #     p("Press 'Predict!' button to make prediction by the model from your parameters. Prediction result will be in right bottom corner of page."),
    #     h3('Results of prediction'),
    #     fluidRow(
    #         column( 1,
    #                 actionButton('predict','Predict!')
    #         ),
    #         column( 6, 
    #                 h4('You entered'),
    #                 verbatimTextOutput("inputValue")
    #         ),
    #         column( 5, 
    #                 h4('Which resulted in a prediction of '),
    #                 verbatimTextOutput("prediction")
    #         )
    #     ),
    #     p("Note: Sepal.Width and Sepal.Length hold constantly = 0 as they do not affect the model.")
))

