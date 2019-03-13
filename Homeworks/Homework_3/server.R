library(shiny) # Load shiny package
library(tidyverse)
library(gridExtra)
rm(list = ls())
shinyServer(
  function(input, output) {
    data1 <- reactive({
      n <- 1000
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
    
    # Correlogram
    data2 <- reactive({
      n<-500
      x<-c()
      for(i in 1:n){
        x[i]=input$intercept1^(i-1)
        print(x[i])
      }
      y<-c()
      y[1] = input$intercept2 / (1 + input$intercept2^2)
      for(i in 2:n){
        y[i]=0
      }
      
      tibble(t=seq(0,n-1, length=n), x, y)
    })
    
    output$myplot1 <- renderPlot({
      p1 <- ggplot(data1(), aes(t, x), xlab=format(t, "%Y")) + geom_line()
      p2 <- ggplot(data2(), aes(t))+ geom_point(aes(y=x)) + xlim(0, 5)
      grid.arrange(grobs=list(p1, p2), ncol = 2)
    }
    )
    output$myplot2 <- renderPlot({
      p1 <- ggplot(data1(), aes(t, y), xlab=format(t, "%Y")) + geom_line()
      p2 <- ggplot(data2(), aes(t))+ geom_point(aes(y=y)) + xlim(0, 5)
      grid.arrange(grobs=list(p1, p2), ncol = 2)
      
    }
    )
    
  }
)
