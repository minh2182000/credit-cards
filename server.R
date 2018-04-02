library(shiny)
library(sqldf); library(boot); library(caret); library(class); library(e1071); library(plotly); library(randomForest); library(nnet)
credit = read.csv("credit.csv", header = TRUE)

# server
shinyServer(function(input, output){
  source("clean.R")
  source("learning.R")
  source("plot.R")
  
  # --- this is for reactive selection input of bank and card --------------
  output$bank_selector = renderUI({
    selectInput("Bank", "Select Bank: ", choices = sort(unique(credit$Bank)), selected = "Chase")
  })
  output$card_selector = renderUI({
    selectInput("Card", "Select Card: ", choices = sort(unique(credit$Card.Name[credit$Bank == input$Bank])), selected = "Freedom")
  })
  #---- calculation -------------
  calc = eventReactive(input$gobutton,{
    learning.out = learning(bank = input$Bank,
                            card = input$Card,
                            Score = input$score,
                            Age_month = input$Age,
                            Income = input$Income,
                            New3 = input$New3,
                            New6 = input$New6,
                            New12 = input$New12)
    return(learning.out)
  })
  plot_out = eventReactive(input$gobutton,{
    plotout = myplot(input$Card, input$score, input$Age, input$Income)
    return(plotout)
  })
  
  help1 = eventReactive(input$gobutton,{return("(assuming you call for recon if fail for instant approval)")})
  
  #----output ---------------
  output$Instant = renderText({
    out = calc()
    out$I.out
  })
  output$Final = renderText({
    out = calc()
    out$F.out
  })
  output$helptext1  = renderText({help1()})
  
  output$plot = renderPlotly({plot_out()})
  
  output$accu_LR_I = renderText({
    out = calc()
    paste("Logistic Regression accuracy: ", round(out$LR.I.accuracy, 4))
  })
  output$accu_LR_F = renderText({
    out = calc()
    paste("Logistic Regression accuracy: ", round(out$LR.F.accuracy, 4))
  })
  output$accu_KNN_I = renderText({
    out = calc()
    paste("KNN accuracy: ", round(out$KNN.I.accuracy, 4))
  })
  output$accu_KNN_F = renderText({
    out = calc()
    paste("KNN accuracy: ", round(out$KNN.F.accuracy, 4))
  })
  output$accu_RF_I = renderText({
    out = calc()
    paste("Random Forest accuracy: ", round(out$RF.I.accuracy, 4))
  })
  output$accu_RF_F = renderText({
    out = calc()
    paste("Random Forest accuracy: ", round(out$RF.F.accuracy, 4))
  })
  output$accu_NN_I = renderText({
    out = calc()
    paste("Neural Network accuracy: ", round(out$NN.I.accuracy, 4))
  })
  output$accu_NN_F = renderText({
    out = calc()
    paste("Neural Network accuracy: ", round(out$NN.F.accuracy, 4))
  })
})



