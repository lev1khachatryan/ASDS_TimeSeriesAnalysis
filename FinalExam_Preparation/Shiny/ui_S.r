# Install the shiny package if not already installed
# install.packages("shiny")

library(shiny) # load the shiny package

# Define UI for application
shinyUI(fluidPage(
  
  # Header or title Panel 
  titlePanel(h4('Demostration of the Spurious Regression with tabs', align = "center")),
  
  # Sidebar panel
  sidebarPanel(
    numericInput("nvariance1", "Sigma 1:", 2, min=-0, max=100),
    numericInput("slope", "Slope:", 0.5, min=-0, max=100)),
   
    
    # Main Panel
  mainPanel(
    tabsetPanel(type = "tab", tabPanel("AR(1)",plotOutput("myplot1"),plotOutput("myplot2")), tabPanel ("MA(1)",plotOutput("myplot3"),plotOutput("myplot4")))
  )     
  )
  )
  