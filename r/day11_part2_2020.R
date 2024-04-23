directions <- list(list(c(-1, -1)), list(c(0, -1)), list(c(1, -1)), 
                   list(c(-1, 0)), list(c(1, 0)), 
                   list(c(-1, 1)), list(c(0, 1)), list(c(1, 1)))

simulateSeatingPartTwo <- function(seatingArea) {
  rows <- nrow(seatingArea)
  cols <- ncol(seatingArea)
  newSeatingArea <- seatingArea
  stabilized <- TRUE
  
  for (i in 1:rows) {
    for (j in 1:cols) {
      switch(seatingArea[i, j],
             "L" = {
               if (countVisibleOccupied(seatingArea, i, j) == 0) {
                 newSeatingArea[i, j] <- "#"
                 stabilized <- FALSE
               }
             },
             "#" = {
               if (countVisibleOccupied(seatingArea, i, j) >= 5) {
                 newSeatingArea[i, j] <- "L"
                 stabilized <- FALSE
               }
             })
    }
  }
  
  return(list(newSeatingArea, stabilized))
}

countVisibleOccupied <- function(seatingArea, row, col) {
  count <- 0
  for (dir in directions) {
    r <- row + dir[[1]][1]
    c <- col + dir[[1]][2]
    while (r >= 1 && r <= nrow(seatingArea) && c >= 1 && c <= ncol(seatingArea)) {
      if (seatingArea[r, c] == "L") {
        break
      }
      if (seatingArea[r, c] == "#") {
        count <- count + 1
        break
      }
      r <- r + dir[[1]][1]
      c <- c + dir[[1]][2]
    }
  }
  return(count)
}

countOccupiedSeats <- function(seatingArea) {
  count <- 0
  for (i in 1:nrow(seatingArea)) {
    for (j in 1:ncol(seatingArea)) {
      if (seatingArea[i, j] == "#") {
        count <- count + 1
      }
    }
  }
  return(count)
}

readInput <- function() {
  input <- readLines("input.txt")
  seatingArea <- matrix(nrow = length(input), ncol = nchar(input[1]))
  for (i in 1:length(input)) {
    seatingArea[i, ] <- strsplit(input[i], "")[[1]]
  }
  return(seatingArea)
}

seatingArea <- readInput()
stabilized <- FALSE
while (!stabilized) {
  result <- simulateSeatingPartTwo(seatingArea)
  seatingArea <- result[[1]]
  stabilized <- result[[2]]
}

print(countOccupiedSeats(seatingArea))