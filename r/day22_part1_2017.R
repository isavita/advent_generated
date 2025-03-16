
lines <- readLines("input.txt")
grid <- list()
startY <- 0
startX <- 0

for (y in seq_along(lines)) {
  line <- trimws(lines[y])
  for (x in seq_along(strsplit(line, "")[[1]])) {
    if (substr(line, x, x) == '#') {
      grid[[paste(x - 1, y - 1, sep = ",")]] <- TRUE
    }
  }
  startX <- nchar(line) %/% 2
  startY <- (y - 1) %/% 2
}

dx <- c(0, 1, 0, -1)
dy <- c(-1, 0, 1, 0)
x <- startX
y <- startY
dir <- 0
infectedCount <- 0

for (i in 1:10000) {
  pos <- paste(x, y, sep = ",")
  if (isTRUE(grid[[pos]])) {
    dir <- (dir + 1) %% 4
    grid[[pos]] <- NULL
  } else {
    dir <- (dir - 1 + 4) %% 4
    grid[[pos]] <- TRUE
    infectedCount <- infectedCount + 1
  }
  x <- x + dx[dir + 1]
  y <- y + dy[dir + 1]
}

cat(infectedCount)
