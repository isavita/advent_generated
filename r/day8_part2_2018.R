readInput <- function(filename) {
  as.integer(unlist(strsplit(readLines(filename), " ")))
}

parseTree <- function(data, index) {
  childCount <- data[index]
  metaCount <- data[index + 1]
  index <- index + 2
  
  childValues <- integer(childCount)
  for (i in seq_len(childCount)) {
    result <- parseTree(data, index)
    childValues[i] <- result$value
    index <- result$index
  }
  
  value <- 0
  if (childCount == 0) {
    value <- sum(data[index:(index + metaCount - 1)])
  } else {
    for (i in seq_len(metaCount)) {
      metadata <- data[index + i - 1]
      if (metadata > 0 && metadata <= childCount) {
        value <- value + childValues[metadata]
      }
    }
  }
  index <- index + metaCount
  
  list(value = value, index = index)
}

main <- function() {
  numbers <- readInput("input.txt")
  result <- parseTree(numbers, 1)
  print(result$value)
}

main()