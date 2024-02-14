data <- readLines("input.txt")
steps <- strsplit(data, ",")[[1]]

x <- 0
y <- 0
max_dist <- 0

for (step in steps) {
  if (step == "n") {
    y <- y + 1
  } else if (step == "s") {
    y <- y - 1
  } else if (step == "ne") {
    x <- x + 1
  } else if (step == "sw") {
    x <- x - 1
  } else if (step == "nw") {
    x <- x - 1
    y <- y + 1
  } else if (step == "se") {
    x <- x + 1
    y <- y - 1
  }
  
  dist <- (abs(x) + abs(y) + abs(x + y)) / 2
  if (dist > max_dist) {
    max_dist <- dist
  }
}

print(dist)