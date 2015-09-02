dashboardPage(skin="blue",
              
  dashboardHeader(title = "Control Panel"),
  
  dashboardSidebar(
    
    h2("Settings:"),
    br(),br(),
    radioButtons(
      "functionSelect", "Select Function:",
      c("Decrypt Message", "Encrypt Message", "Encrypt and Decrypt Message")
    ),
    br(),br(),
    sliderInput("levDist", "Word Variant Sensitivity:", 1, 5, 4)
  ),
  
  dashboardBody(
   
    fluidRow(
      box(
        h1("Welcome to the Culper Code Translator!"),
        h2("Here's how it works:"),
        br(),
        p("Use the ", strong("Control Panel"), " on the left to set your parameters."),
        p(strong("Select Function"), " allows you to choose whether your input should be
          decrypted, encrypted, or encrypted and then decrypted. The last option is useful
          if you want to compare your expected output with the actual output."),
        p(strong('Word Variant Sensitivity'), " lets you set how sensitive 
          the function that encrypts messages should be when it looks for word variants. 
          The lower this number is, the more variants it wll catch, but it will also find more 
          false positives. The opposite it true as the number gets higher. Use the ",
          strong("Encrypt and Decrypt Message"), "function to compare inputs and 
          outputs until you find the sensitivity level you're comfortable with!"),
        p("The number you set using ", strong("Word Variant Sensitivity"), " represents a 
          maximum levenshtein distance for the input strings and their potential variants. 
          More information about the levenshtein distance", a("can be found at this link.",
                                                              href = "https://en.wikipedia.org/wiki/Levenshtein_distance") ),
        width = 12,
        solidHeader = TRUE,
        status = "primary"
      )
    ),
    
    fluidRow(
      box(
        h3("Your Original Message:"),
        textInput(inputId = "origMessage", label = NULL, value = "444"),
        actionButton(inputId = "translateButton", label = "Translate!", icon = icon("gear")),
        width = 6,
        solidHeader = TRUE,
        status = "primary"
        ),
      
      box(
        h3("Your Translated Message:"),
        br(),
        textOutput("newMessage"),
        width = 6,
        solidHeader = TRUE,
        status = "primary"
        )
    ),
    
    includeCSS("www/custom.css")
    
  )
)