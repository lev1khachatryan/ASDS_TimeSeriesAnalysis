library(shiny) # Load shiny package
library(tidyverse)
library(gridExtra)
rm(list = ls())
shinyServer(
  function(input, output) {
    data1 <- reactive({
      n <- 100
      x <- c()
      x[1] <- 1
      for(i in 2:n){
        x[i] <- input$intercept1 * x[i-1] + rnorm(1,0,input$nvariance1)
      }
      y <- c()
      e <- rnorm(n, mean = 0, sd = input$nvariance2)
      y[1] <- e[1]
      for(i in 2:n){
        y[i] <- e[i] + input$intercept2 * e[i-1]
      }
      tibble(t=seq(1,n, length = n), x, y)
    })
    
    output$myplot1 <- renderPlot({
      ggplot(data1(), aes(t, x), xlab=format(t, "%Y")) + geom_line()
    }
    )
    output$myplot3 <- renderPlot({
      ggplot(data1(), aes(t, y), xlab=format(t, "%Y")) + geom_line()
    }
    )
    output$myplot2 <- renderPlot({
      n <- 100
      x <- c()
      x[1] <- 1
      for(i in 2:n){
        x[i] <- input$intercept1 * x[i-1] + rnorm(1,0,input$nvariance1)
      }
      acf(x)
    }
    )
    output$myplot4 <- renderPlot({
      n <- 100
      y <- c()
      e <- rnorm(n, mean = 0, sd = input$nvariance2)
      y[1] <- e[1]
      for(i in 2:n){
        y[i] <- e[i] + input$intercept2 * e[i-1]
      }
      acf(y)
    }
    )
    output$myplot5 <- renderPlot({
      n <- 100
      x <- c()
      x[1] <- 1
      for(i in 2:n){
        x[i] <- input$intercept1 * x[i-1] + rnorm(1,0,input$nvariance1)
      }
      pacf(x)
    }
    )
    output$myplot6 <- renderPlot({
      n <- 100
      y <- c()
      e <- rnorm(n, mean = 0, sd = input$nvariance2)
      y[1] <- e[1]
      for(i in 2:n){
        y[i] <- e[i] + input$intercept2 * e[i-1]
      }
      pacf(y)
    }
    )
  }
)



pnorm(2.5) - pnorm(-0.5)
