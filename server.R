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
      encrypt.message(input$origMessage, levDist)
    }
    
    else {
      decrypt.message(encrypt.message(input$origMessage, levDist))
    }
  })
  
  output$newMessage <- renderText({
    translate()
  })
})