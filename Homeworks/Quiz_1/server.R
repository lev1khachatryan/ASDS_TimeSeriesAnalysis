library(shiny) # Load shiny package
library(tidyverse)
library(gridExtra)
rm(list = ls())

"Spurious regression 
is a regression that provides misleading statistical evidence of a 
linear relationship between independent non-stationary variables.
Spurious regression is nonsense regression.
The dictionary meaning of spurious is not being what it purports to be 
That means it is fake or false or fabricated. 
In other words, Spurious regression will indicate non existing relationship as if existing. 
When actually there is no causal connection they could have been correlated with each other.
This happens when R2 is typically very high and t value is significant
We carry out regression analysis using stationary variables.
The means variances, co- variances change with time .
Here the coefficient will be zero or near zero that means the two series data are independent.
Spurious regression becomes possible when there are two local trends which are similar 
but it may not be true though they move together.
It is necessary to test the data for stationary variables and co-integration 
before establishing time series regression correlation.
In short, when regression analysis is carried out using non stationary variables, 
there could be possibility of false or nonsense or spurious regression. 
It is indispensable in such cases to check whether the residual is non stationary.
Regression models for non-stationary variables impart spurious results only exception is when the model avoids the stochastic trends.
"
shinyServer(
  function(input, output) {
    data1 <- reactive({
      n <- 1000
      x <- c()
      x[1] <- 1
      for(i in 2:n){
        x[i] <- x[i-1] + rnorm(1,0,input$nvariance1)
      }
      sim1 <- tibble(t=seq(1,n, length=n), x)
      y <- c()
      y[1] <- 1
      for(i in 2:n){
        y[i] <- y[i-1] + rnorm(1,0,input$nvariance2)
      }
      sim2 <- tibble(t=seq(1,n, length=n), y)
      tibble(t=seq(1,n, lenght = n), x, y)
      #data <- sim1%>%mutate(sim2$y)
    })
    output$myplot <- renderPlot({
      p1 <- ggplot(data1(), aes(t)) + geom_line(aes(y=x, colour = "x")) + geom_line(aes(y=y, colour= "y"))
      p2 <- ggplot(data1(), aes(x,y)) + geom_point() + geom_smooth(method = "lm", se = F)
      grid.arrange(p1, p2, ncol=2)
    }
    )
    output$myRsquare <- renderText({
      out <- summary(lm(x~y, data1()))
      out$r.squared
    })
  }
)
