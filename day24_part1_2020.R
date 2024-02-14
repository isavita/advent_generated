
input <- readLines("input.txt")

directions <- list(
  e = c(1, 0),
  se = c(0, 1),
  sw = c(-1, 1),
  w = c(-1, 0),
  nw = c(0, -1),
  ne = c(1, -1)
)

blackTiles <- list()
for (line in input) {
  coord <- c(0, 0)
  i <- 1
  while (i <= nchar(line)) {
    dir <- substr(line, i, i)
    if (dir %in% c("e", "w")) {
      dir <- substr(line, i, i)
    } else {
      dir <- substr(line, i, i + 1)
      i <- i + 1
    }
    move <- directions[[dir]]
    coord <- coord + move
    i <- i + 1
  }
  key <- paste(coord, collapse = ",")
  if (is.null(blackTiles[[key]])) {
    blackTiles[[key]] <- TRUE
  } else {
    blackTiles[[key]] <- !blackTiles[[key]]
  }
}

count <- sum(unlist(blackTiles))
cat(count, "\n")
