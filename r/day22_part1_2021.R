read_input <- function() {
  input <- readLines("input.txt")
  rebootSteps <- list()
  for (line in input) {
    if (line != "") {
      parts <- strsplit(line, " ")[[1]]
      action <- parts[1]
      coords <- strsplit(parts[2], ",")[[1]]
      xRange <- strsplit(coords[1], "=")[[1]][2]
      yRange <- strsplit(coords[2], "=")[[1]][2]
      zRange <- strsplit(coords[3], "=")[[1]][2]
      xStart <- as.integer(strsplit(xRange, "\\..")[[1]][1])
      xEnd <- as.integer(strsplit(xRange, "\\..")[[1]][2])
      yStart <- as.integer(strsplit(yRange, "\\..")[[1]][1])
      yEnd <- as.integer(strsplit(yRange, "\\..")[[1]][2])
      zStart <- as.integer(strsplit(zRange, "\\..")[[1]][1])
      zEnd <- as.integer(strsplit(zRange, "\\..")[[1]][2])
      rebootSteps <- c(rebootSteps, list(list(action, xStart, xEnd, yStart, yEnd, zStart, zEnd)))
    }
  }
  return(rebootSteps)
}

create_cube_grid <- function(minCoord, maxCoord) {
  gridSize <- maxCoord - minCoord + 1
  grid <- array(FALSE, dim = c(gridSize, gridSize, gridSize))
  return(grid)
}

execute_reboot_steps <- function(cubeGrid, rebootSteps) {
  for (step in rebootSteps) {
    if (!(step[[2]] >= -50 && step[[3]] <= 50 && step[[4]] >= -50 && step[[5]] <= 50 && step[[6]] >= -50 && step[[7]] <= 50)) {
      next
    }
    for (x in step[[2]]:step[[3]]) {
      for (y in step[[4]]:step[[5]]) {
        for (z in step[[6]]:step[[7]]) {
          cubeGrid[x+51, y+51, z+51] <- step[[1]] == "on"
        }
      }
    }
  }
  return(cubeGrid)
}

count_on_cubes <- function(cubeGrid) {
  count <- sum(cubeGrid)
  return(count)
}

rebootSteps <- read_input()
minCoord <- -50
maxCoord <- 50
cubeGrid <- create_cube_grid(minCoord, maxCoord)
cubeGrid <- execute_reboot_steps(cubeGrid, rebootSteps)
onCubes <- count_on_cubes(cubeGrid)
print(onCubes)