#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
# Define UI for application that draws a histogram
pageWithSidebar(
  headerPanel("Beräkning av kontrollgruppsstorlek för att bevisa en skillnad mellan grupperna"),
  sidebarPanel("Teststyrka",
      numericInput("test","Antal test", value = 1),
      sliderInput("sig1", "Signifikansnivå",
                  min = 1, max = 99, value = 95),
      sliderInput("Power", "Power", min=0.01, max=0.99, value=0.8),
      "Mål och kontrollgrupp 1",
      numericInput("pros1","Totalt antal prospekts",value=10000),
      numericInput("hitrate1", "Hit rate målgrupp", value=0.01),
      numericInput("hiteratec1", "Hit rate kontrollgruppen", value=0.005),
      "Mål och kontrollgrupp 2",
      numericInput("pros2","Totalt antal prospekts",value=NA),
      numericInput("hitrate2", "Hit rate målgrupp", value=NA),
      numericInput("hiteratec2", "Hit rate kontrollgruppen", value=NA)             
      
    )
  
  ,
  mainPanel(
    
    tableOutput("trams" )
  
    
))