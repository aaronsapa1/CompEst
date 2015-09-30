
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(parallel)
require(plyr)
library(ggplot2)
library(dplyr)


mc <- function (Phi, N, X.dens=runif, alpha=0.05){
  result.list <- lapply(N,function(nsim){
   # X <- sapply(nsim, FUN=X.dens)
    X<-runif(N,0,1)
    #X<-X.dens(N)
    PhiX   <- sapply(X,Phi)
    estim <- mean(PhiX)
    S2 <- var(PhiX)
    quant <- qnorm(alpha/2,lower.tail=FALSE)
    int.upper <- estim + sqrt(S2/nsim)*quant
    int.lower <- estim - sqrt(S2/nsim)*quant
    return(data.frame(N = nsim, Estimate = estim, LI = int.lower, UI = int.upper))
  })
  results.table <- ldply(result.list) %>% mutate(i = row_number())
  return (results.table)
}

mc.importanceSampling <- function (Phi, N, X.dens=runif,g=dunif, alpha=0.05){
  result.list <- lapply(N,function(nsim){
    #X <- sapply(nsim, FUN=X.dens)
    #X<-runif(nsim,0,1)
    #X<-rexp(nsim,rat=4)
    X<- X.dens(N)
    w <- sapply(X,g)
    PhiX   <- sapply(X,Phi)
    estim <- mean(PhiX/w)
    S2 <- var(PhiX)
    quant <- qnorm(alpha/2,lower.tail=FALSE)
    int.upper <- estim + sqrt(S2/nsim)*quant
    int.lower <- estim - sqrt(S2/nsim)*quant
    return(data.frame(N = nsim, Estimate = estim, LI = int.lower, UI = int.upper))
  })
  results.table <- ldply(result.list)%>% mutate(i = row_number())
  return (results.table)
}

raw<-function(x){
  runif(x,0,2)
}
shinyServer(function(input, output) {

    N <- reactive(seq(100,input$N,by=100))
    
    raw.data<-reactive({
      phi <- function(x){
       input$m*exp(-(input$m)*x)
      } 
      
      mc(phi,N())   
    })
    
    is.exp<-reactive({
      phi <- function(x){
        input$m*exp(-(input$m)*x)
      }
      exp.is<-function(x){
        U <- runif(x, 0, 1) 
        X <- -log(1 - (1 - exp(-2*(input$lambda)))*U)
      }
      g=function(x){
        dexp(x)/(1-exp(-2*(input$lambda)))
      }
      mc.importanceSampling(phi,N(),X.dens=exp.is,g=g)
      
    })

    is.beta<-reactive({
      phi <- function(x){
        input$m*exp(-(input$m)*x)
      }
      beta.is<-function(x){
        U <- runif(x, 0, 1)
        rbeta(U,input$a,input$b)
      } 
      g=function(x){
        dbeta(x,input$a,input$b)
      }
      mc.importanceSampling(phi,N(),X.dens=beta.is,g=g)
    })
    lambda<- reactive({
      phi <- function(x){
        input$m*exp(-(input$m)*x)
      }
      exp.is<-function(x,lambda){
        U <- runif(x, 0, 1) 
        X <- -log(1 - (1 - exp(-2*(input$lambda)))*U)
      }
      g=function(x,lambda){
        dexp(x)/(1-exp(-2*(lambda)))
      }
      lambda.v<-seq(.1,3*input$m,by=0.1)
      result<-lapply(lambda.v,function(y){
          X<- exp.is(input$N,y)
          w <- g(X,y)
          PhiX   <- sapply(X,phi)
          estim <- mean(PhiX/w)
          return(estim)
      })
      results.table <- ldply(result) %>% mutate(l = lambda.v,Estim=V1)
    })
    output$lambdag<-renderPlot({
      dat<- lambda() %>% mutate(method='lambda')
      ggplot(dat) + 
        geom_line(aes(l,Estim)) + 
        geom_hline(yintercept=1-exp(-2*input$m),linetype="dashed") +
        geom_vline(xintercept=input$m,linetype="dashed") +
        geom_vline(xintercept=2*input$m,linetype="dashed")
      
    })
    
    output$raw <- renderPlot({
      dat <- raw.data() %>% mutate(i = row_number(), method='x ~ raw')
      ggplot(dat) +
        geom_ribbon(aes(i,ymin=LI,ymax=UI, fill=method), alpha=0.5) +
        geom_line(aes(i,Estimate)) +
        facet_wrap(~method, nrow = 2) + geom_hline(yintercept=1-exp(-2*input$m),linetype="dashed")
    })
   
    output$exp <- renderPlot({
      dat <- is.exp() %>% mutate(i = row_number(), method='x ~ exp')
      ggplot(dat) +
        geom_ribbon(aes(i,ymin=LI,ymax=UI, fill=method), alpha=0.5) +
        geom_line(aes(i,Estimate)) +
        facet_wrap(~method, nrow = 2) + geom_hline(yintercept=1-exp(-2*input$m),linetype="dashed")
    })
    output$beta <- renderPlot({
      dat <- is.beta() %>% mutate(i = row_number(), method='x ~ beta')
      ggplot(dat) +
        geom_ribbon(aes(i,ymin=LI,ymax=UI, fill=method), alpha=0.5) +
        geom_line(aes(i,Estimate)) +
        facet_wrap(~method, nrow = 2) + geom_hline(yintercept=1-exp(-2*input$m),linetype="dashed")
    })
    
    output$data<-renderDataTable(lambda())
})
