
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

x <- function(...){
  u1 <- runif(1)
  u2 <- runif(1)
  sqrt(-2*log(u2))*cos(2*pi*u1)  
}
y <- function(...){
  u1 <- runif(1)
  u2 <- runif(1)
  sqrt(-2*log(u2))*sin(2*pi*u1)  
}
shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x.1 <- sapply(numeric(input$X),x)
    y.1 <-sapply(numeric(input$Y),y)

    # draw the histogram with the specified number of bins
    hist(x.1, col = 'darkgray', border = 'white')
   
  })
  output$yplot <- renderPlot({
    y.1 <-sapply(numeric(input$Y),y)
    
    # draw the histogram with the specified number of bins
    hist(y.1, col = 'darkgray', border = 'white')
    
  })

})
