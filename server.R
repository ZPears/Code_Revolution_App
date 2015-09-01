shinyServer(function(input,output) {
  
  source("helpers.R")

  translate <- eventReactive(input$translateButton, {
    input$origMessage
  })
  
  output$newMessage <- renderText({
    translate()
  })
})