
library(e1071)
library(shiny)
library(caret)
library(ggplot2)



# fit the model
fit <- train(Species ~ .,
             method = "rpart2", data = iris, tuneGrid = data.frame(maxdepth=4))

shinyServer(function(input, output) {
    
    output$pwPlot <- renderPlot({
        ggplot(data = iris, aes(Species, Petal.Width)) + geom_boxplot() + geom_hline(y = input$Petal.Width, color="red")
    })
    
    output$plPlot <- renderPlot({
        ggplot(data = iris, aes(Species, Petal.Length)) + geom_boxplot() + geom_hline(y = input$Petal.Length, color="red")
    })
#     output$slPlot <- renderPlot({
#         ggplot(data = iris, aes(Species, Sepal.Length)) + geom_boxplot() + geom_hline(y = input$Sepal.Length, color="red")
#     })
    
    # keep Sepal.Length and Sepal.Width constant as they do not affect the result
    newdata <- reactive({data.frame(Petal.Width = input$Petal.Width, 
                                    Petal.Length = input$Petal.Length,
                                    Sepal.Length = 0,
                                    Sepal.Width = 0)})    
    
    output$inputValue <- renderPrint({
        input$predict
        isolate(newdata())
        })
    
    output$prediction <- renderPrint({
        input$predict
        isolate(as.character(predict(fit, newdata())))
        })
    
})