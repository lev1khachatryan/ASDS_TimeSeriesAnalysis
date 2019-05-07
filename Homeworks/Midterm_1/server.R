library(shiny) # Load shiny package
library(tidyverse)
rm(list = ls())

watchlist=c("IBM", 'GE')
getSymbols(watchlist,from="2018-01-01",to="2019-01-01",periodicity = 'daily' )

IBM <- merge(IBM,dailyReturn(IBM))
GE <- merge(GE,dailyReturn(GE))

shinyServer(
  function(input, output) {
    data1 <- reactive({
      R <- input$weight * IBM$daily.returns + (1-input$weight)*GE$daily.returns
      print(head(R))
      t <- index(R)
      d <- data.frame(R)
      d <- d$daily.returns
      tibble(d, t)
    })
    output$myplot <- renderPlot({
      ggplot(data1(), aes(d,t ))+ geom_line()+ geom_smooth(method = "lm", se=TRUE)
      # head(R)
    }
    )
  }
)
