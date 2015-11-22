library(shiny)

shinyUI(fluidPage(
    
    headerPanel("Species prediction with CART"),
    p("Application uses caret package to predict Species as output by Petal.Width and Petal.Length based on iris dataset."),
    p("Model fited using rpart2 method as follows:"),
    code("fit <- train(Species ~ ., 
         method = 'rpart2', data = iris, tuneGrid = data.frame(maxdepth=4))"),
    p("Use two sliders below to adjust imput parameters to model. Box plots at the left indicating your input according to iris dataset"),
    fluidRow(
        column( 4,
                sliderInput("Petal.Width", 
                            "Petal Width:", 
                            min = 0.1,
                            max = 2.5, 
                            step = 0.1,
                            value = 1.3)
        ),
        column( 8, 
                plotOutput("pwPlot", height = 220)
        )
    ),
    fluidRow(
        column( 4,
                sliderInput("Petal.Length", 
                            "Petal Length:", 
                            min = 1,
                            max = 7, 
                            step = 0.1,
                            value = 4)
        ),
        column( 8, 
                plotOutput("plPlot", height = 220)
        )
    ),
    hr(),
    p("Press 'Predict!' button to make prediction by the model from your parameters. Prediction result will be in right bottom corner of page."),
    h3('Results of prediction'),
    fluidRow(
        column( 1,
                actionButton('predict','Predict!')
        ),
        column( 6, 
                h4('You entered'),
                verbatimTextOutput("inputValue")
        ),
        column( 5, 
                h4('Which resulted in a prediction of '),
                verbatimTextOutput("prediction")
        )
    ),
    p("Note: Sepal.Width and Sepal.Length hold constantly = 0 as they do not affect the model.")
))

