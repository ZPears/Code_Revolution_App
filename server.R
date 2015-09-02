shinyServer(function(input,output) {
  
  source("helpers.R")

  #reactive functions
  funcSelect <- reactive({
    input$functionSelect
  })
  
  levDist <- reactive({
    input$levDist
  })
  
  #reactive events
  translate <- eventReactive(input$translateButton, {
    if (funcSelect() == "Decrypt Message") {
      decrypt.message(input$origMessage)
    }
    
    else if (funcSelect() == "Encrypt Message") {
      encrypt.message(input$origMessage, input$levDist)[1]
    }
    
    else {
      decrypt.message(encrypt.message(input$origMessage, input$levDist)[1])
    }
  })
  
  output$newMessage <- renderText({
    translate()
  })
})