data <- read.csv("Culper Codes - Sheet1.csv", stringsAsFactors = FALSE)

data[767,1] <- "A"
data[584,2] <- "reinforcement"

numericToWordCipher <- data[1:765,]
letterToLetterCipher <- data[767:792,]
letterToNumericCipher <- data[794:803,]

capWords <- numericToWordCipher[grep("([A-Z][a-z.]+ )", numericToWordCipher$Word),2]

capWords <- sapply(capwords, function(word) {
  strsplit(word, " ")[[1]][1]
})

capWords <- unique(as.vector(capwords))

# FUNCTIONS BELOW, DATA DEFINITIONS ABOVE

detect.variant <- function(word, levDist) {
  word <- tolower(word)

  if (nchar(word) <= (levDist)) {
    return(NULL)
  }
  
  narrowedCipher <- numericToWordCipher[grep(paste0("^", substr(word,1,1)), numericToWordCipher$Word),]
  narrowedCipher <- narrowedCipher[nchar(narrowedCipher$Word) > nchar(word) - levDist,]
  narrowedCipher <- narrowedCipher[nchar(narrowedCipher$Word) < nchar(word) + levDist,]
 
  if (nchar(word) < 6) {
    xChars <- 3
  } else {
    xChars <- nchar(word) - 3
  }
  
  narrowedCipher <- narrowedCipher[substr(narrowedCipher$Word,1,xChars) == substr(word,1,xChars),]
  
  if (length(narrowedCipher$Word) == 0) {
    return(NULL)
  } else if (length(narrowedCipher$Word) == 1) {
    return(narrowedCipher$Numeric.Code[1])
  } else {
    levenshtein(narrowedCipher, word, levDist)
  }
  
}

levenshtein <- function(narrowedCipher, word, levDist) {
  levDist <- levDist
  answer <- ""

  for (value in narrowedCipher$Word) {
    value <- tolower(value)
    newLeast <- 0
    
    word <- strsplit(word, "")[[1]]
    value <- strsplit(value, "")[[1]]
    
    if (length(word) > length(value)) {
      longer <- word
    } else {
      longer <- value
    }
    
    for (counter in (1:length(longer))) {
      if (is.na(word[counter]) || is.na(value[counter])) {
        newLeast <- newLeast + 1
      } else if (word[counter] != value[counter]) {
        newLeast <- newLeast + 1
      }
    }
    
    if (newLeast == 1) {
      return(paste(value, collapse=""))
    } else if (newLeast < levDist) {
      answer <- narrowedCipher[narrowedCipher$Word == paste(value, collapse=""),]
      levDist <- newLeast
    }
    
    return(answer)
    
  }
}

check.for.errors <- function(word) {
  if (grepl("([a-zA-Z])", word) && grepl("([0-9])", word)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

decrypt.message <- function(message) {
  message <- strsplit(message, " ")[[1]]
  newMessage <- ""
  
  for (word in message) {
    if (substr(word,1,1) == "~") {
      word <- strsplit(word, "~")[[1]][2]
    }
    
    if (check.for.errors(word)) {
      return("INPUT_ERROR: do not mix letters and numbers in the same word. Please try again.")
    }
    
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

encrypt.message <- function(plaintext, levDist) {
  plaintext <- strsplit(plaintext, " ")[[1]]
  newMessage <- ""
  
  textIndex <- 1
  
  for (word in plaintext) {
    if (grepl("([A-Z])", substr(word,1,1)) && (word %in% capWords)) {
      plaintext[textIndex] <- paste(plaintext[textIndex], 
                                    " ", 
                                    plaintext[textIndex], 
                                    sep="")
      plaintext <- plaintext[-(textIndex+1)]
    }
    textIndex <- textIndex+1
  }
  
  for (word in plaintext) {
  
    if (check.for.errors(word)) {
      return("INPUT_ERROR: do not mix letters and numbers in the same word. Please try again.")
    }
    
    else if (!is.na(as.integer(word))) {
      newWord <- sapply(strsplit(word, split="")[[1]], function(char) {
        word <- letterToNumericCipher[grep(char, letterToNumericCipher$Numeric.Code),2]
      })
      newWord <- paste(newWord, collapse="")
      newWord <- paste("_", newWord, "_", sep="")
      newMessage <- paste(newMessage, newWord, sep=" ")
      
    }
    
    else if (word %in% numericToWordCipher$Word) {
      newWord <- numericToWordCipher[
        grep(paste0("(^", word, "$)"),
        numericToWordCipher$Word),1
      ]
      newMessage <- paste(newMessage, newWord, sep=" ")
    }
    
    else {
      
      guess <- detect.variant(word, levDist)
      if (!is.null(guess)) {
        newMessage <- paste(newMessage, " ~", guess, " ", sep="")
      } else {
        newWord <- sapply(strsplit(toupper(word), split="")[[1]], function(char) {
          word <- tolower(letterToLetterCipher[grep(char, letterToLetterCipher$Numeric.Code),2])
        })
        newWord <- paste(newWord, collapse="")
        newMessage <- paste(newMessage, newWord, sep=" ")
      }  
      
    }

  }
  
  substr(newMessage,2,nchar(newMessage))
  
}