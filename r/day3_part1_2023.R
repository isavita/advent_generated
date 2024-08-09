readFileToMatrix <- function(filePath) {
  matrix <- readLines(filePath)
  strsplit(matrix, "")
}

extractNumber <- function(matrix, x, y) {
  numberStr <- ""
  while (x <= length(matrix[[y]]) && grepl("[0-9]", matrix[[y]][x])) {
    numberStr <- paste0(numberStr, matrix[[y]][x])
    x <- x + 1
  }
  list(as.numeric(numberStr), nchar(numberStr))
}

checkAdjacent <- function(matrix, x, y) {
  for (dy in -1:1) {
    for (dx in -1:1) {
      adjX <- x + dx
      adjY <- y + dy
      if (adjY >= 1 && adjY <= length(matrix) && adjX >= 1 && adjX <= length(matrix[[adjY]])) {
        if (!grepl("[0-9]", matrix[[adjY]][adjX]) && matrix[[adjY]][adjX] != ".") {
          return(TRUE)
        }
      }
    }
  }
  FALSE
}

sumOfPartNumbers <- function(matrix) {
  sum <- 0
  visited <- matrix(FALSE, nrow = length(matrix), ncol = max(sapply(matrix, length)))
  
  for (y in seq_along(matrix)) {
    for (x in seq_along(matrix[[y]])) {
      if (!visited[y, x] && grepl("[0-9]", matrix[[y]][x])) {
        res <- extractNumber(matrix, x, y)
        number <- res[[1]]
        length <- res[[2]]
        if (any(sapply(0:(length-1), function(i) checkAdjacent(matrix, x + i, y)))) {
          sum <- sum + number
        }
        visited[y, x:(x + length - 1)] <- TRUE
      }
    }
  }
  sum
}

matrix <- readFileToMatrix("input.txt")
result <- sumOfPartNumbers(matrix)
print(result)