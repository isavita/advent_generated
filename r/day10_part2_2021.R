
checkAndCompleteLine <- function(line, pairings, scoreValues, opening, closing) {
  stack <- c()
  
  for (char in strsplit(line, "")[[1]]) {
    if (char %in% opening) {
      stack <- c(stack, char)
    } else if (char %in% closing) {
      if (length(stack) == 0 || tail(stack, 1) != pairings[char]) {
        return(list(score = 0, incomplete = FALSE))
      }
      stack <- stack[-length(stack)]
    }
  }
  
  if (length(stack) == 0) {
    return(list(score = 0, incomplete = FALSE))
  }
  
  score <- 0
  for (i in length(stack):1) {
    score <- score * 5 + scoreValues[getClosingChar(stack[i])]
  }
  return(list(score = score, incomplete = TRUE))
}

getClosingChar <- function(openingChar) {
  switch(openingChar,
         "(" = ")", "[" = "]", "{" = "}", "<" = ">",
         " ")
}

scores <- c()
input <- readLines("input.txt", warn = FALSE)  # Added warn = FALSE to suppress warning
pairings <- c(")" = "(", "]" = "[", "}" = "{", ">" = "<")
scoreValues <- c(")" = 1, "]" = 2, "}" = 3, ">" = 4)
opening <- c("(", "[", "{", "<")
closing <- c(")", "]", "}", ">")

for (line in input) {
  result <- checkAndCompleteLine(line, pairings, scoreValues, opening, closing)
  if (result$incomplete) {
    scores <- c(scores, result$score)
  }
}

middleScore <- median(scores, na.rm = TRUE)  # Added na.rm = TRUE to handle NA values
cat(middleScore)
