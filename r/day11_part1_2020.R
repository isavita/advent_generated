
input <- readLines("input.txt")
input <- input[input != ""]  # Remove empty lines from input

seats <- strsplit(input, "")
seats <- lapply(seats, unlist)
seats <- do.call(rbind, seats)

adjacent_seats <- function(seats, i, j) {
  adjacent <- c(-1, 0, 1)
  count <- 0
  for (dx in adjacent) {
    for (dy in adjacent) {
      if (!(dx == 0 && dy == 0) && i + dx >= 1 && i + dx <= nrow(seats) && j + dy >= 1 && j + dy <= ncol(seats)) {
        if (seats[i + dx, j + dy] == "#") {
          count <- count + 1
        }
      }
    }
  }
  return(count)
}

change_seat <- function(seats, i, j) {
  if (seats[i, j] == "L" && adjacent_seats(seats, i, j) == 0) {
    return("#")
  } else if (seats[i, j] == "#" && adjacent_seats(seats, i, j) >= 4) {
    return("L")
  } else {
    return(seats[i, j])
  }
}

while (TRUE) {
  new_seats <- seats
  for (i in 1:nrow(seats)) {
    for (j in 1:ncol(seats)) {
      new_seats[i, j] <- change_seat(seats, i, j)
    }
  }
  if (identical(seats, new_seats)) {
    break
  } else {
    seats <- new_seats
  }
}

occupied_seats <- sum(sapply(seats, function(x) sum(x == "#")))
print(occupied_seats)
