#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)


#Mål- kontrollgrupp1
function(input, output){
  textinput <- reactive({
    res1 <-  (-qnorm(1-input$sig1/100)+qnorm(input$Power))^2
    res2 <-  input$hitrate1*(1-input$hitrate1)+input$hiteratec1*(1-input$hiteratec1)
    res3 <- (input$hitrate1-input$hiteratec1)^2
    res4 <- round(res1*res2/res3,0)
    res5 <- input$pros1 - res4
    alpha <- (1-input$sig1/100)
    kontrollgrupp <-as.integer(ifelse(res5 < res4, input$pros1/2, res4))
    malgrupp <- as.integer(input$pros1-kontrollgrupp)
    
    tab <- as.data.frame(cbind(malgrupp, kontrollgrupp))
    names(tab) <- c("Målgrupp 1", "Kontrollgrupp 1")
    return(tab)
  })
  output$trams <- renderTable({textinput()})
  
  ### Mål- kontrolgrupp2
  textinput2 <- reactive({
    res1 <-  (-qnorm(1-input$sig1/100)+qnorm(input$Power))^2
    res2 <-  input$hitrate2*(1-input$hitrate2)+input$hiteratec2*(1-input$hiteratec2)
    res3 <- (input$hitrate2-input$hiteratec2)^2
    res4 <- round(res1*res2/res3,0)
    res5 <- input$pros2 - res4
    alpha <- (1-input$sig1/100)
    kontrollgrupp <-as.integer(ifelse(res5 < res4, input$pros2/2, res4))
    malgrupp <- as.integer(input$pros2-kontrollgrupp)
    
    tab <- as.data.frame(cbind(malgrupp, kontrollgrupp))
    names(tab) <- c("Målgrupp 2", "Kontrollgrupp 2")
    return(tab)
  })
  output$trams1 <- renderTable({textinput2()})
  
}
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)

# Define server logic required to draw a histogram
function(input, output){
  textinput <- reactive({
    res1 <-  (-qnorm(1-input$sig1/100)+qnorm(input$Power))^2
    res2 <-  input$hitrate1*(1-input$hitrate1)+input$hiteratec1*(1-input$hiteratec1)
    res3 <- (input$hitrate1-input$hiteratec1)^2
    res4 <- round(res1*res2/res3,0)
    res5 <- input$pros1 - res4
    alpha <- (1-input$sig1/100)
    
    kontrollgrupp <-as.integer(ifelse(res5 < res4, input$pros1/2, res4))
    malgrupp <- as.integer(input$pros1-kontrollgrupp)
    
    tab <- as.data.frame(cbind(malgrupp, kontrollgrupp))
    names(tab) <- c("Målgrupp 1", "Kontrollgrupp 1")
    return(tab)
    
  })
  
  textinput1 <- reactive({
    res1 <-  (-qnorm(1-input$sig1/100)+qnorm(input$Power))^2
    res2 <-  input$hitrate2*(1-input$hitrate2)+input$hiteratec2*(1-input$hiteratec2)
    res3 <- (input$hitrate2-input$hiteratec2)^2
    res4 <- round(res1*res2/res3,0)
    res5 <- input$pros2 - res4
    alpha <- (1-input$sig1/100)
    kontrollgrupp <-as.integer(ifelse(res5 < res4, input$pros2/2, res4))
    malgrupp <- as.integer(input$pros2-kontrollgrupp)
    
    tab <- as.data.frame(cbind(malgrupp, kontrollgrupp))
    names(tab) <- c("Målgrupp 2", "Kontrollgrupp 2")
    return(tab)
  })
  output$trams1 <- renderTable({textinput1()})
  
  
  
  textinput2<- reactive({
    res1 <-  (-qnorm(1-input$sig1/100)+qnorm(input$Power))^2
    res2 <-  input$hitrate1*(1-input$hitrate1)+input$hiteratec1*(1-input$hiteratec1)
    res3 <- (input$hitrate1-input$hiteratec1)^2
    res4 <- round(res1*res2/res3,0)
    res5 <- input$pros1 - res4
    alpha <- (1-input$sig1/100)
    
    kontrollgrupp <-as.integer(ifelse(res5 < res4, input$pros1/2, res4))
    malgrupp <- as.integer(input$pros1-kontrollgrupp)
    
    svar_malgrp<- input$hitrate1*malgrupp  
    svar_cntgrp<- input$hiteratec1*kontrollgrupp
    
    tab2<- as.data.frame(cbind(svar_malgrp, svar_cntgrp))
    names(tab2)<- c("Målgrupp: Förväntade antal svar", "Kontrollgrupp: Förväntade antal svar" )
    
    return(tab2)
    
  })
  
  
  
  output$trams <- renderTable({textinput()})
  output$trams2 <- renderTable({textinput2()})
  
}
