data <- read.csv("Culper Codes - Sheet1.csv", stringsAsFactors = FALSE)

data[767,1] <- "A"
data[584,1] <- "reinforcement"

numericToWordCipher <- data[1:765,]
letterToLetterCipher <- data[767:792,]
letterToNumericCipher <- data[794:803,]

# define capwords

apply(capwords, function() {
  
})

#capwords <- unique(capwords)

detect.variant <- function(word) {
  
}

levenshtein <- function(narrowedCipher, word) {
  
}

check.for.errors <- function(word) {

}

