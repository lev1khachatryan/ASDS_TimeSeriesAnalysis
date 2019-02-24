library(shiny) # Load shiny package
library(tidyverse)

shinyServer(
  
  
  function(input, output) {
    data1 <- reactive({
    n<-50
    x<-c()
    e<-rnorm(n, mean = input$nmean, sd = input$nvariance)
    x[1]<-1
    x[2]<-1
    for(i in 3:n){
      x[i]=input$slope1 * x[i-1] + input$slope2 * x[i-2] + e[i] + input$intercept
    }
    time <- seq(from = as.Date("1970-01-01"), to = as.Date("2019-01-01"), by = 'year')
    series <- ts(x, frequency=1, start=c(1970))
    #year = format(time, "%Y")
    l<-cbind.data.frame(time,x)
    colnames(l)<-cbind("time","series")
    data<-as.tibble(l)
    })
    output$myplot <- renderPlot({
    ggplot(data1() , aes(time, series), xlab=format(time, "%Y")) + geom_line()
    }
    )
    }
    )

