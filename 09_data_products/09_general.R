
# Quiz 1 question 1

library(manipulate)
myPlot <- function(s) {
    plot(cars$dist - mean(cars$dist), cars$speed - mean(cars$speed))
    abline(0, s)
}


manipulate(myPlot(s), s = slider(0, 2, step = 0.1))


# Quiz 1 question 2

install.packages("devtools")
require(devtools)
install_github('rCharts', 'ramnathv')

library(rCharts)

data("airquality")

dTable(airquality, sPaginationType = "full_numbers")



# Quiz 1 question 3

library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Data science FTW!"),
    sidebarPanel(
        h2('Big text'),
        h3('Sidebar')
    ),
    mainPanel(
        h3('Main Panel text')
    )
))

# install shiny

install.packages("shiny")


