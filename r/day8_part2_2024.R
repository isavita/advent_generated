
gcd <- function(a, b) {
  if (b == 0) {
    return(abs(a))
  }
  return(gcd(b, a %% b))
}

input <- readLines("input.txt")
h <- length(input)
w <- nchar(input[1])
antennas <- list()

for (y in 1:h) {
  for (x in 1:w) {
    c <- substr(input[y], x, x)
    if (c != ".") {
      if (is.null(antennas[[c]])) {
        antennas[[c]] <- list()
      }
      antennas[[c]] <- append(antennas[[c]], list(c(y, x)))
    }
  }
}

linesPerFreq <- list()
for (f in names(antennas)) {
  coords <- antennas[[f]]
  n <- length(coords)
  linesPerFreq[[f]] <- list()
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      A <- coords[[i]]
      B <- coords[[j]]
      dy <- B[1] - A[1]
      dx <- B[2] - A[2]
      g <- gcd(dy, dx)
      sy <- dy / g
      sx <- dx / g
      if (sx < 0 || (sx == 0 && sy < 0)) {
        sx <- -sx
        sy <- -sy
      }
      c <- sy * A[2] - sx * A[1]
      key <- paste(sx, sy, c, sep = ",")
      linesPerFreq[[f]][[key]] <- TRUE
    }
  }
}

antinodes <- list()
for (lines in linesPerFreq) {
  for (key in names(lines)) {
    parts <- as.integer(strsplit(key, ",")[[1]])
    sx <- parts[1]
    sy <- parts[2]
    c <- parts[3]
    if (sx == 0 && sy == 0) {
      next
    }
    if (sy == 0) {
      if (c %% sx == 0) {
        y <- -c / sx
        if (y >= 1 && y <= h) {
          for (x in 1:w) {
            antinodes[[paste(y, x, sep = ",")]]=TRUE
          }
        }
      }
    } else if (sx == 0) {
      if (c %% sy == 0) {
        x <- c / sy
        if (x >= 1 && x <= w) {
          for (y in 1:h) {
            antinodes[[paste(y, x, sep = ",")]]=TRUE
          }
        }
      }
    } else {
      for (y in 1:h) {
        val <- c + sx * y
        if (val %% sy == 0) {
          x <- val / sy
          if (x >= 1 && x <= w) {
            antinodes[[paste(y, x, sep = ",")]]=TRUE
          }
        }
      }
    }
  }
}

cat(length(antinodes), "\n")
