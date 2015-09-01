detect.variant <- function(word) {
  
}

levenshtein <- function(narrowedCipher, word) {
  
}

check.for.errors <- function(word) {
  
}

decrypt.message <- function(message) {
  message <- strsplit(message, " ")[[1]]
  newmessage <- ""
  
  sapply(message, function(word) {
    if (substr(word,1,1) == "~") {
      word <- strsplit(word, "~")[[1]][2]
    }
  })
  
  message
}

encrypt.message <- function(plaintext) {
  plaintext <- strsplit(plaintext, " ")[[1]]
}