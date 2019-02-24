# Install the shiny package if not already installed
# install.packages("shiny")

library(shiny) # load the shiny package

# Define UI for application
shinyUI(fluidPage(
  
  # Header or title Panel 
  titlePanel(h4('Demostration of the Time series generation with linear generation function', align = "center")),
  
  # Sidebar panel
  sidebarPanel(
    # numericInput("min", "Min:", 1, min=-100, max=100),
    # numericInput("max", "Max:", 100, min=-0, max=200),
    numericInput("slope1", "Slope1:", 1, min=-100, max=100),
    numericInput("slope2", "Slope2:", 1, min=-100, max=100),
    numericInput("intercept", "Intercept:", 0, min=1, max=100),
    numericInput("nmean", "Normal Mean:", 0, min=1, max=10),
    numericInput("nvariance", "Normal Variance:", 1, min=0.01, max=100)),
    
    # Main Panel
  mainPanel(
        plotOutput("myplot")
  )
  )
  )
