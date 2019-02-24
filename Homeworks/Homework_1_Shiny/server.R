library(shiny) # Load shiny package
library(tidyverse)
rm(list = ls())

shinyServer(
  function(input, output) {
    data1 <- reactive({
      X <- seq(0, 100, length = 50)
      if(input$rb == "sin"){
        Y <- input$intercept +  input$slope * sin(X) + rnorm(n = length(X), mean = input$nmean, sd = input$nvariance)
      }else{
        Y <- input$intercept +  input$slope * cos(X) + rnorm(n = length(X), mean = input$nmean, sd = input$nvariance)
      }
      data <- tibble(X, Y)
    })
    output$myplot <- renderPlot({
      ggplot(data1(), aes(X, Y))+ geom_line()+ geom_smooth(method = "lm", se=TRUE)

    }
    )
  }
)
