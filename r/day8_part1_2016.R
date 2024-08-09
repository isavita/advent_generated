screenWidth <- 50
screenHeight <- 6

screen <- matrix(FALSE, nrow = screenHeight, ncol = screenWidth)

processInstruction <- function(instruction) {
  if (grepl("rect", instruction)) {
    matches <- as.integer(unlist(regmatches(instruction, gregexpr("\\d+", instruction))))
    rect(matches[1], matches[2])
  } else if (grepl("rotate row", instruction)) {
    matches <- as.integer(unlist(regmatches(instruction, gregexpr("\\d+", instruction))))
    rotateRow(matches[1] + 1, matches[2])
  } else if (grepl("rotate column", instruction)) {
    matches <- as.integer(unlist(regmatches(instruction, gregexpr("\\d+", instruction))))
    rotateColumn(matches[1] + 1, matches[2])
  }
}

rect <- function(a, b) {
  screen[1:b, 1:a] <<- TRUE
}

rotateRow <- function(row, shift) {
  screen[row, ] <<- c(tail(screen[row, ], shift), head(screen[row, ], screenWidth - shift))
}

rotateColumn <- function(col, shift) {
  temp <- screen[, col]
  screen[, col] <<- c(tail(temp, shift), head(temp, screenHeight - shift))
}

countLitPixels <- function() {
  sum(screen)
}

input <- readLines("input.txt")
invisible(lapply(input, processInstruction))
cat(countLitPixels(), "\n")