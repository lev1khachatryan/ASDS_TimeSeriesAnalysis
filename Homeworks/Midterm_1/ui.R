# Install the shiny package if not already installed
# install.packages("shiny")
library(shiny)
shinyUI(fluidPage(
  titlePanel(h4('daily return for portfolio with different values of weight.', align = "center")),
  sidebarPanel(
    numericInput("weight", "Weight: ", 0.5, min=0, max=1)
  ),
  mainPanel(
        plotOutput("myplot")
  )
)
)
