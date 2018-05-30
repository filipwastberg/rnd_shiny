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
    res1<-  (-qnorm(1-input$sig1/100)+qnorm(input$Power))^2
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
  
  
  textinput4<- reactive({
    res1 <-  (-qnorm(1-input$sig1/100)+qnorm(input$Power))^2
    res2 <-  input$hitrate1*(1-input$hitrate1)+input$hiteratec1*(1-input$hiteratec1)
    res3 <- (input$hitrate1-input$hiteratec1)^2
    res4 <- round(res1*res2/res3,0)
    res5 <- input$pros1 - res4
    alpha <- (1-input$sig1/100)
    
    kontrollgrupp <-as.integer(ifelse(res5 < res4, input$pros1/2, res4))
    malgrupp <- as.integer(input$pros1-kontrollgrupp)
    
    
    res1_2<-  (-qnorm(1-input$sig1/100)+qnorm(input$Power))^2
    res2_2 <-  input$hitrate2*(1-input$hitrate2)+input$hiteratec2*(1-input$hiteratec2)
    res3_2 <- (input$hitrate2-input$hiteratec2)^2
    res4_2 <- round(res1_2*res2_2/res3_2,0)
    res5_2 <- input$pros2 - res4_2
    alpha2 <- (1-input$sig1/100)
    kontrollgrupp2 <-as.integer(ifelse(res5_2 < res4_2, input$pros2/2, res4_2))
    malgrupp2 <- as.integer(input$pros2-kontrollgrupp2)
    
    p_joint<- ifelse(malgrupp2 == 0, 0, 
                     ((input$hitrate2 * malgrupp + input$hitrate2 * malgrupp2)/(malgrupp2 + malgrupp)))
    z<- ifelse(malgrupp2 ==0, 0,
               ((input$hitrate2 - input$hitrate2 - 0)/((p_joint*(1-p_joint)*(1/malgrupp2 + 1/malgrupp))^0.5)))
    z_grans<- qnorm(1-alpha/2)
    #prob_null <- ifelse(z>0, (1-pnorm(abs(z)))^2, 0 )
    
    reject_null<- ifelse(z<z_grans, "SANT", "FALSKT")
    
    
    tab2<- as.data.frame(cbind(p_joint, reject_null))
    names(tab2)<- c("p joint", "Skillnad mellan Målgrupp1 och Målgrupp2 kan detekteras" )
    
    return(tab2)
    
  })
  
  
  output$trams <- renderTable({textinput()})
  output$trams1 <- renderTable({textinput1()})
  output$trams4 <- renderTable({textinput4()})
  
}




