library(shiny) # Load shiny package
library(tidyverse)
library(tseries)
shinyServer(
  function(input, output) {
    data1 <- reactive({
      n <- 1000
      sigma1 <- 5
      AR1 <- c()
      for(i in 2:n){
        AR1[1] <- 1
        AR1[i]<-input$slope*AR1[i-1]+rnorm(1,0,input$nvariance1)
      }
      tibble(t=seq(0,n, length=n), AR1)
    })
    data2 <- reactive({
      n <- 1000
      MA1 <- c()
      al <- rnorm(1000,0,input$nvariance1)
      for(i in 2:n){
        MA1[1]<-al[1]
        MA1[i]<-al[1]+ input$slope*al[i-1]
      }
      tibble(t=seq(0,n, length=n), MA1)
    })
    output$myplot1 <- renderPlot({
      ggplot(data1(), aes(t, AR1))+ geom_line()
    }
    )
    output$myplot2 <- renderPlot({
      n <- 1000
      sigma1 <- 5
      AR1 <- c()
      for(i in 2:n){
        AR1[1] <- 1
        AR1[i]<-input$slope*AR1[i-1]+rnorm(1,0,input$nvariance1)
      }
      tibble(t=seq(0,n, length=n), AR1)
      acf(AR1)
    
    }
    )
    output$myplot3 <- renderPlot({
      ggplot(data2(), aes(t, MA1))+ geom_line()
    }
    )
    output$myplot4 <- renderPlot({
      n <- 1000
      MA1 <- c()
      al <- rnorm(1000,0,input$nvariance1)
      for(i in 2:n){
        MA1[1]<-al[1]
        MA1[i]<-al[1]+ input$slope*al[i-1]
      }
      tibble(t=seq(0,n, length=n), MA1)
      acf(MA1)
    }
    )
   
  }
)
