cubes <- list()
surfaceArea <- 0

input <- read.csv("input.txt", header = FALSE, stringsAsFactors = FALSE)
for (i in 1:nrow(input)) {
  x <- as.integer(input[i, 1])
  y <- as.integer(input[i, 2])
  z <- as.integer(input[i, 3])
  cubes <- c(cubes, list(list(x, y, z)))
}

for (cube in cubes) {
  exposedSides <- 6
  for (dir in list(c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0), c(0, -1, 0), c(0, 0, 1), c(0, 0, -1))) {
    adjacent <- c(cube[[1]] + dir[1], cube[[2]] + dir[2], cube[[3]] + dir[3])
    if (any(sapply(cubes, function(x) all(x == adjacent)))) {
      exposedSides <- exposedSides - 1
    }
  }
  surfaceArea <- surfaceArea + exposedSides
}

print(surfaceArea)