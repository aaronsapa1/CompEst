
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

LCG<-function(nsim,M=(2^32),a=1664525,b=1013904223,x0=1){
  X = c(x0,numeric(nsim-1))
  for(i in 1:(nsim-1)){
    X[i+1]<-(a*X[i]+b) %% M
  }
  return (X/M)
}


shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- LCG(input$nsim)
    l <- input$lambda
    exp <- sapply(x,function(y){-1*(log(1-y)/l)})

    # draw the histogram with the specified number of bins
    hist(exp, col = 'darkgray', border = 'white')
    tabl<-t(t(exp))
    output$table <- renderTable(tabl)
  })

})
