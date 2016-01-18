library(shiny)

shinyUI(fluidPage(
    
    tags$head(tags$style("#wordLinks{color: orange;
                                 font-size: 30px;
                                font-style: bold;}")
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
        
        # dataTableOutput("details")
        # uiOutput("advanced")
        conditionalPanel("input.isAdvanced == true", 
                         column(6,
                                hr(), 
                                dataTableOutput("details")),
                         column(6, 
                                p("Advanced mode. In table below you can find additional information about prediction result."),
                                p("Table cintains searching result by last n-gram where n is from 4 to 1."),
                                p(strong("Feild description:")),
                                p(strong("prefix")," - ngram prefix from language model. Word count in prefix is between 3 and 1"),
                                p(strong("lastw"), " - last word of ngram"),
                                p(strong("freq"), " - frequancy of ngram including last word"),
                                p(strong("l_freq"), " - last word own frequency"),
                                p(strong("nlength"), " - count of word in prefix")
                         )
                         
        )
    )
))

