houses <- readLines("input.txt")
x <- 0
y <- 0
houses_visited <- list(paste(x, y))

for (i in 1:nchar(houses)) {
  direction <- substr(houses, i, i)
  if (direction == "^") {
    y <- y + 1
  } else if (direction == "v") {
    y <- y - 1
  } else if (direction == ">") {
    x <- x + 1
  } else if (direction == "<") {
    x <- x - 1
  }
  houses_visited[[length(houses_visited) + 1]] <- paste(x, y)
}

unique_houses_visited <- unique(houses_visited)
houses_visited_count <- length(unique_houses_visited)
print(houses_visited_count)