readFirstRow <- function(filename) {
  readLines(filename, n = 1)
}

countSafeTiles <- function(firstRow, totalRows) {
  safeCount <- sum(strsplit(firstRow, "")[[1]] == ".")
  currentRow <- firstRow
  
  for(i in 1:(totalRows - 1)) {
    nextRow <- ""
    for(j in 1:nchar(currentRow)) {
      left <- ifelse(j - 1 < 1, ".", substr(currentRow, j - 1, j - 1))
      center <- substr(currentRow, j, j)
      right <- ifelse(j + 1 > nchar(currentRow), ".", substr(currentRow, j + 1, j + 1))
      
      if((left == "^" && center == "^" && right == ".") ||
         (center == "^" && right == "^" && left == ".") ||
         (left == "^" && center == "." && right == ".") ||
         (right == "^" && center == "." && left == ".")) {
        nextRow <- paste0(nextRow, "^")
      } else {
        nextRow <- paste0(nextRow, ".")
        safeCount <- safeCount + 1
      }
    }
    currentRow <- nextRow
  }
  safeCount
}

firstRow <- readFirstRow("input.txt")
safeTilesCount <- countSafeTiles(firstRow, 40)
print(safeTilesCount)