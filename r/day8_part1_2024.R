
grid <- readLines("input.txt")
h <- length(grid)
w <- nchar(grid[1])
antennas <- list()
for (y in 1:h) {
  for (x in 1:w) {
    c <- substr(grid[y], x, x)
    if (c != ".") {
      if (is.null(antennas[[c]])) {
        antennas[[c]] <- matrix(c(y, x), ncol = 2)
      } else {
        antennas[[c]] <- rbind(antennas[[c]], c(y, x))
      }
    }
  }
}

antinodes <- list()
for (coords in antennas) {
  n <- nrow(coords)
  if(n > 1){
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        A <- coords[i,]
        B <- coords[j,]
        P1 <- 2 * A - B
        P2 <- 2 * B - A
        if (P1[1] > 0 && P1[1] <= h && P1[2] > 0 && P1[2] <= w) {
          antinodes[[paste(P1, collapse = ",")]]=TRUE
        }
        if (P2[1] > 0 && P2[1] <= h && P2[2] > 0 && P2[2] <= w) {
          antinodes[[paste(P2, collapse = ",")]]=TRUE
        }
      }
    }
  }
}

cat(length(antinodes), "\n")
