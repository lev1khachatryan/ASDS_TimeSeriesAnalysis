# Install the shiny package if not already installed
# install.packages("shiny")
library(shiny)
shinyUI(fluidPage(
  titlePanel(h4('Simulation of nonstationary in variance series', align = "center")),
  sidebarPanel(
    numericInput("slope1", "Slope1:", 0.5, min=-100, max=100),
    numericInput("slope2", "Slope2:", -0.5, min=-100, max=100),
    numericInput("nvariance1", "Normal Variance 1:", 2, min=0.01, max=100),
    numericInput("nvariance2", "Normal Variance 2:", 7, min=0.01, max=100)
  ),
  mainPanel(
        plotOutput("myplot")
  )
)
)
