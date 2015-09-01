data <- read.csv("Culper Codes - Sheet1.csv", stringsAsFactors = FALSE)

data[767,1] <- "A"
data[584,1] <- "reinforcement"

numericToWordCipher <- data[1:765,]
letterToLetterCipher <- data[767:792,]
letterToNumericCipher <- data[794:803,]

detect.variant <- sapply(splitWord, function(char) {
  word <- letterToNumericCipher[grep(char, letterToNumericCipher$Word),1]
})

levenshtein <- function(narrowedCipher, word) {
  
}

check.for.errors <- function(word) {
  
}

decrypt.message <- function(message) {
  message <- strsplit(message, " ")[[1]]
  newMessage <- ""
  
  #change to for loop. sapply doesn't persist information
  
  for (word in message) {
    if (substr(word,1,1) == "~") {
      word <- strsplit(word, "~")[[1]][2]
    }
    
    # check.for.errors(word)
    
    if (substr(word,1,1) == "_") {
      word <- substr(word,2,nchar(word)-1)
      newWord <- sapply(word, function(char) {
        word <- letterToNumericCipher[grep(char, letterToNumericCipher$Word),1]
      })
      newWord <- paste(names(newWord), collapse="")
        #for (char in strsplit(word, "")[[1]]) {
        #  newWord <- paste(newWord, 
        #                   letterToNumericCipher[grep(char, letterToNumericCipher$Word),1],
        #                   sep="")
        #}
      print(newWord)
      newMessage <- paste(newMessage, newWord)
      print(newMessage)
    } #else if (word.downcase == is letter) {
      
    #} else if (#word is integer != NA) & word is integer < 764) {
      
    #} else {
      
    #}
    
  }
  
  print(newMessage)
}

encrypt.message <- function(plaintext) {
  plaintext <- strsplit(plaintext, " ")[[1]]
}