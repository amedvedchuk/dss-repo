
library(shiny)
# source("cap_functions.R")

MAX_LINKS = 10

takeLess <- function(num1, num2){
    if(num1>num2){
        num2
    } else {
        num1
    }
}

getMaxLinks <- function(len, show_last){
    min(MAX_LINKS, len, show_last)
}

# dtl4 <- readRDS("dtl4_from_2016-01-11_ng1_with_lastw.rds")

shinyServer(function(input, output, session) {
    
    
    predicted <- reactive({predict_backoff(dtl4, input$phrase, simple_out = F, show_last = input$show_last)})   
    
    output$nrows <- reactive(nrow(predicted()))
    
    output$details <- renderDataTable({
        # predict_backoff(dtl4, newdata(),simple_out = F, show_last = 3)
        predicted()
    })
    
    output$wordLinks <- renderUI({
        # print("renderUI")
        i <- 1
        lapply(predicted()$lastw[1:getMaxLinks(nrow(predicted()), input$show_last)], function(word){
            # print(i)
            al <- actionLink(paste("word",i,sep = ""), label = word)
            i <<- i+1
            al
        })
        
    })

    #         reactiveValues()
    #         isolate()
    #         observe()
    
    observe({
        
        genObserver <- function(index){

            varName <- paste("word", index, sep = "")
            input[[varName]]
            
            if(!is.null(input[[varName]]) && input[[varName]]>0){
                isolate({
                    print(paste("from click:", varName))
                    updateTextInput(session, "phrase",
                                    value = paste(input$phrase, predicted()$lastw[index])
                    )
                })
            }
        }
        
        lapply(1:getMaxLinks(isolate(nrow(predicted())), input$show_last), genObserver)
        
    }, label = "my_obs")
    
})