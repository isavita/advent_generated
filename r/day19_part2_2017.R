grid <- readLines("input.txt")
grid <- strsplit(grid, split = "")

x <- which(grid[[1]] == "|")[1]  # Start at the first pipe found
y <- 1
dx <- 0
dy <- 1
steps <- 0

while (y >= 1 && y <= length(grid) && x >= 1 && x <= length(grid[[y]])) {
  cell <- grid[[y]][x]

  if (cell == " ") break
  steps <- steps + 1

  if (cell == "+") {
    if (dx == 0) {
      if (x > 1 && grid[[y]][x - 1] %in% c("-", LETTERS)) {
        dx <- -1; dy <- 0
      } else {
        dx <- 1; dy <- 0
      }
    } else {
      if (y > 1 && grid[[y - 1]][x] %in% c("|", LETTERS)) {
        dx <- 0; dy <- -1
      } else {
        dx <- 0; dy <- 1
      }
    }
  }
  
  x <- x + dx
  y <- y + dy
}

print(steps)