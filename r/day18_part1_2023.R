
parse_input <- function(input) {
  dirs <- list(
    U = c(0, -1),
    L = c(-1, 0),
    D = c(0, 1),
    R = c(1, 0)
  )
  current <- c(0, 0)
  vertices <- list(current)
  for (line in input) {
    parts <- strsplit(line, " ")[[1]]
    dir_input <- substr(parts[1], 1, 1)
    length <- as.integer(parts[2])
    dir <- dirs[[dir_input]]
    current <- current + dir * length
    vertices <- append(vertices, list(current))
  }
  vertices
}

shoelace <- function(vertices) {
  n <- length(vertices)
  area <- 0
  for (i in 1:n) {
    next_i <- (i %% n) + 1
    area <- area + vertices[[i]][1] * vertices[[next_i]][2]
    area <- area - vertices[[i]][2] * vertices[[next_i]][1]
  }
  abs(area) / 2
}

perimeter <- function(vertices) {
  n <- length(vertices)
  perim <- 0
  for (i in 1:n) {
    next_i <- (i %% n) + 1
    perim <- perim + abs(vertices[[i]][1] - vertices[[next_i]][1]) + abs(vertices[[i]][2] - vertices[[next_i]][2])
  }
  perim
}

calculate_polygon_area <- function(vertices) {
  shoelace(vertices) + perimeter(vertices) / 2 + 1
}

solve <- function(input) {
  vertices <- parse_input(input)
  calculate_polygon_area(vertices)
}

input <- readLines("input.txt")
result <- solve(input)
cat(result, "\n")
