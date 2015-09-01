shinyServer(function(input,output) {
  
  source("helpers.R")

  #reactive functions
  funcSelect <- reactive({
    input$functionSelect
  })
  
  #reactive events
  translate <- eventReactive(input$translateButton, {
    if (funcSelect() == "Decrypt Message") {
      decrypt.message(input$origMessage)
    }
  })
  
  output$newMessage <- renderText({
    translate()
  })
})