
data <- readLines("input.txt")
wire1 <- strsplit(data[1], ",")[[1]]
wire2 <- strsplit(data[2], ",")[[1]]

get_points <- function(path) {
  x <- 0
  y <- 0
  points <- matrix(nrow = 0, ncol = 2)
  for (move in path) {
    dir <- substr(move, 1, 1)
    steps <- as.integer(substr(move, 2, nchar(move)))
    for (i in 1:steps) {
      if (dir == "U") y <- y + 1
      else if (dir == "D") y <- y - 1
      else if (dir == "L") x <- x - 1
      else if (dir == "R") x <- x + 1
      points <- rbind(points, c(x, y))
    }
  }
  unique(points)
}

points1 <- get_points(wire1)
points2 <- get_points(wire2)

intersections <- merge(as.data.frame(points1), as.data.frame(points2), by = c("V1", "V2"))

distances <- abs(intersections$V1) + abs(intersections$V2)
min_distance <- min(distances)

cat(min_distance)
