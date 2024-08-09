readInput <- function(filename) {
  readLines(filename, warn = FALSE)[1]
}

incrementPassword <- function(password) {
  chars <- strsplit(password, "")[[1]]
  for (i in length(chars):1) {
    new_char <- intToUtf8((utf8ToInt(chars[i]) + 1))
    if (new_char > "z") {
      chars[i] <- "a"
    } else {
      chars[i] <- new_char
      break
    }
  }
  paste(chars, collapse = "")
}

isValidPassword <- function(password) {
  hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password)
}

hasStraight <- function(password) {
  for (i in 1:(nchar(password) - 2)) {
    if (utf8ToInt(substr(password, i, i)) + 1 == utf8ToInt(substr(password, i + 1, i + 1)) &&
        utf8ToInt(substr(password, i + 1, i + 1)) + 1 == utf8ToInt(substr(password, i + 2, i + 2))) {
      return(TRUE)
    }
  }
  FALSE
}

containsInvalidLetters <- function(password) {
  any(strsplit(password, "")[[1]] %in% c("i", "o", "l"))
}

hasTwoPairs <- function(password) {
  count <- 0
  chars <- strsplit(password, "")[[1]]
  i <- 1
  while (i < length(chars)) {
    if (chars[i] == chars[i + 1]) {
      count <- count + 1
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  count >= 2
}

findNextPassword <- function(password) {
  repeat {
    password <- incrementPassword(password)
    if (isValidPassword(password)) {
      return(password)
    }
  }
}

currentPassword <- readInput("input.txt")
newPassword <- findNextPassword(currentPassword)
cat(newPassword, "\n")