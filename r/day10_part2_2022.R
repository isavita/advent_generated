x <- c(1)
lines <- readLines("input.txt")

for (line in lines) {
  if (line == "noop") {
    x <- c(x, tail(x, 1))
  } else {
    n <- as.integer(sub("addx ", "", line))
    x <- c(x, tail(x, 1), tail(x, 1) + n)
  }
}

grid <- matrix(".", nrow = 6, ncol = 40)

for (i in seq_along(x)) {
  crtx <- (i - 1) %% 40
  crty <- (i - 1) %/% 40
  if (abs(crtx - x[i]) <= 1) {
    grid[crty + 1, crtx + 1] <- "#"
  }
}

cat(apply(grid, 1, paste, collapse = ""), sep = "\n")