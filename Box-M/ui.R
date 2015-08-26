
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("X",
                  "X:",
                  min = 1,
                  max = 5000,
                  value = 30),
      sliderInput("Y",
                  "Y:",
                  min = 1,
                  max = 5000,
                  value = 30)
    
 
  ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
      tabPanel("X",plotOutput("distPlot")),
      tabPanel("Y",plotOutput("yplot"))
    ))
  )
))
