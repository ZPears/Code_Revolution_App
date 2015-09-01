data <- read.csv("Culper Codes - Sheet1.csv", stringsAsFactors = FALSE)

data[767,1] <- "A"
data[584,2] <- "reinforcement"

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
  
  for (word in message) {
    if (substr(word,1,1) == "~") {
      word <- strsplit(word, "~")[[1]][2]
    }
    
    # check.for.errors(word)
    
    if (substr(word,1,1) == "_") {
      word <- substr(word,2,nchar(word)-1)
      newWord <- sapply(strsplit(word, split="")[[1]], function(char) {
        word <- letterToNumericCipher[grep(char, letterToNumericCipher$Word),1]
      })
      newWord <- paste(newWord, collapse="")
      newMessage <- paste(newMessage, newWord, sep=" ")
    } 
    
    else if (grepl("(^[a-z]+$)", tolower(word))) {
      newWord <- sapply(strsplit(word, split="")[[1]], function(char) {
        word <- tolower(letterToLetterCipher[grep(char, letterToLetterCipher$Word),1])
      })
      newWord <- paste(newWord, collapse="")
      newMessage <- paste(newMessage, newWord, sep=" ")
    } 
    
    else if (!is.na((as.integer(word))) && (as.integer(word) < 764)) {
      newWord <- numericToWordCipher[
        grep(paste0("(^", word, "$)"),
        numericToWordCipher$Numeric.Code),2
        ]
      newMessage <- paste(newMessage, newWord, sep = " ")
    }
    
    else {
      newMessage <- paste(newMessage, "VALUE_MISSING", sep = " ")
    }
    
  }
  
  substr(newMessage,2,nchar(newMessage))
  
}

encrypt.message <- function(plaintext) {
  plaintext <- strsplit(plaintext, " ")[[1]]
}