# Install the shiny package if not already installed
# install.packages("shiny")

library(shiny) # load the shiny package
# rm(list = ls())

# Define UI for application
shinyUI(fluidPage(
  
  # Header or title Panel 
  titlePanel(h4('Demostration of the AR(1) and MA(1) processes with tabs', align = "center")),
  
  # Sidebar panel
  sidebarPanel(
    numericInput("nvariance1", "Sigma for AR:", 2, min=-0, max=100),
    numericInput("nvariance2", "Sigma for MA:", 2, min=-0, max=100),
    numericInput("intercept1", "Intercept for AR:", 0.9, min=-1, max=1),
    numericInput("intercept2", "Intercept for MA:", 0.9, min=-1, max=1)),
  
  # Main Panel
  mainPanel(
    tabsetPanel(type = "tab", tabPanel("AR(1)",plotOutput("myplot1"),plotOutput("myplot2"), plotOutput("myplot5")), tabPanel ("MA(1)",plotOutput("myplot3"),plotOutput("myplot4"), plotOutput("myplot6")))
  )
)
)
