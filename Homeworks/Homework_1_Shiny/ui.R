# Install the shiny package if not already installed
# install.packages("shiny")
library(shiny)
shinyUI(fluidPage(
  titlePanel(h4('Demostration of the Time series generation', align = "center")),
  sidebarPanel(
    radioButtons("rb", "Distribution type:",
                 c("sin" = "sin",
                   "cos" = "cos")),
    numericInput("slope", "Slope:", 1, min=-100, max=100),
    numericInput("intercept", "Intercept:", 0, min=1, max=100),
    numericInput("nmean", "Normal Mean:", 0, min=1, max=10),
    numericInput("nvariance", "Normal Variance:", 1, min=0.01, max=100)
  ),
  mainPanel(
        plotOutput("myplot")
  )
)
)
