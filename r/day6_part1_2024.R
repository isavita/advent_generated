
grid <- readLines("input.txt")
grid <- strsplit(grid, "")
h <- length(grid)
w <- length(grid[[1]])
dirs <- list(c(0, -1), c(1, 0), c(0, 1), c(-1, 0))
found <- FALSE
for (i in 1:h) {
  if (found) break
  for (j in 1:w) {
    if (found) break
    char <- grid[[i]][j]
    if (char == '^') {
      x <- j
      y <- i
      dirIdx <- 1
      dirX <- dirs[[dirIdx]][1]
      dirY <- dirs[[dirIdx]][2]
      found <- TRUE
    } else if (char == '>') {
      x <- j
      y <- i
      dirIdx <- 2
      dirX <- dirs[[dirIdx]][1]
      dirY <- dirs[[dirIdx]][2]
      found <- TRUE
    } else if (char == 'v') {
      x <- j
      y <- i
      dirIdx <- 3
      dirX <- dirs[[dirIdx]][1]
      dirY <- dirs[[dirIdx]][2]
      found <- TRUE
    } else if (char == '<') {
      x <- j
      y <- i
      dirIdx <- 4
      dirX <- dirs[[dirIdx]][1]
      dirY <- dirs[[dirIdx]][2]
      found <- TRUE
    }
  }
}
visited <- matrix(FALSE, nrow = h, ncol = w)
visited[y, x] <- TRUE
while (TRUE) {
  nx <- x + dirX
  ny <- y + dirY
  if (nx < 1 || nx > w || ny < 1 || ny > h) {
    break
  }
  if (grid[[ny]][nx] == '#') {
    dirIdx <- (dirIdx %% 4) + 1
    dirX <- dirs[[dirIdx]][1]
    dirY <- dirs[[dirIdx]][2]
    next
  }
  x <- nx
  y <- ny
  visited[y, x] <- TRUE
}
print(sum(visited))
