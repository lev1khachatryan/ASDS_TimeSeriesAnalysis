library(shiny) # Load shiny package
library(tidyverse)
rm(list = ls())

shinyServer(
  function(input, output) {
    data1 <- reactive({
      # X <- seq(0, 100, length = 50)
      # if(input$rb == "sin"){
      #   Y <- input$intercept +  input$slope * sin(X) + rnorm(n = length(X), mean = input$nmean, sd = input$nvariance)
      # }else{
      #   Y <- input$intercept +  input$slope * cos(X) + rnorm(n = length(X), mean = input$nmean, sd = input$nvariance)
      # }
      # data <- tibble(X, Y)
      n_1 <- 500
      n_2 <- 1500
      sim <- c()
      sim[1] <- 1
      for(i in 2:n_1){
        sim[i] <- input$slope1*sim[i-1] + rnorm(1,0,input$nvariance1)
      }
      sim[n_1+1] <- 1
      for(i in (n_1+2):n_2){
        sim[i] <- input$slope2*sim[i-1] + rnorm(1,0,input$nvariance2)
      }
      data <- tibble(t=seq(0,n_2, length=n_2), sim)
    })
    output$myplot <- renderPlot({
      ggplot(data1(), aes(t, sim))+ geom_line()

    }
    )
  }
)
