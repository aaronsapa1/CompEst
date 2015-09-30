
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
      sliderInput('N', 'Número de simulaciones',
                  value = 500, min = 500, max = 10000, step = 1),
      sliderInput('m', 'm',
                  value = 2, min = 0.1, max = 5, step = .1),
      sliderInput('lambda', 'lambda',
                  value = 10, min = 0.1, max = 15, step = .1),
      sliderInput('a', 'shape1',
                  value = 1, min = 1, max = 50, step = 1),
      sliderInput('b', 'shape2',
                  value = 20, min = 1, max = 50, step = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('Crudo',plotOutput('raw', width = '8in', height = '6in')),
        tabPanel('Exponencial',plotOutput('exp', width = '8in', height = '6in')),
        tabPanel('Beta',plotOutput('beta', width = '8in', height = '6in')),
        tabPanel('Lambda', plotOutput('lambdag', width = '8in', height = '6in'))
        ,tabPanel('Data g1', dataTableOutput('data'))
        #tabPanel('Data g2', dataTableOutput('is_data')
        )
      )
    )
    )
  )
