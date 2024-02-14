
grid <- readLines("input.txt")
x <- which(strsplit(grid[1], "")[[1]] == "|")[1]
y <- 1

dx <- 0
dy <- 1

letters <- c()

while (TRUE) {
  if (x < 1 || x > nchar(grid[1]) || y < 1 || y > length(grid)) {
    break
  }
  
  cell <- substr(grid[y], x, x)
  
  if (cell == " ") {
    break
  }
  
  if (cell >= "A" && cell <= "Z") {
    letters <- c(letters, cell)
  }
  
  if (cell == "+") {
    if (dx == 0) {
      if (x > 1 && (substr(grid[y], x-1, x-1) == "-" || (substr(grid[y], x-1, x-1) >= "A" && substr(grid[y], x-1, x-1) <= "Z"))) {
        dx <- -1
        dy <- 0
      } else {
        dx <- 1
        dy <- 0
      }
    } else {
      if (y > 1 && (substr(grid[y-1], x, x) == "|" || (substr(grid[y-1], x, x) >= "A" && substr(grid[y-1], x, x) <= "Z"))) {
        dx <- 0
        dy <- -1
      } else {
        dx <- 0
        dy <- 1
      }
    }
  }
  
  x <- x + dx
  y <- y + dy
}

cat(letters, sep = "")
