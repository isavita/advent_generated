readReindeerDetails <- function(filename) {
  reindeers <- list()
  file <- readLines(filename)
  for (line in file) {
    parts <- strsplit(line, "\\s+")[[1]]
    name <- parts[1]
    speed <- as.integer(parts[4])
    flyTime <- as.integer(parts[7])
    restTime <- as.integer(parts[14])
    reindeers <- c(reindeers, list(list(name = name, speed = speed, flyTime = flyTime, restTime = restTime, distance = 0, flying = TRUE, timeInMode = 0)))
  }
  return(reindeers)
}

simulateRace <- function(reindeers, totalSeconds) {
  for (i in 1:totalSeconds) {
    for (j in 1:length(reindeers)) {
      reindeer <- reindeers[[j]]
      if (reindeer$flying) {
        reindeer$distance <- reindeer$distance + reindeer$speed
        reindeer$timeInMode <- reindeer$timeInMode + 1
        if (reindeer$timeInMode == reindeer$flyTime) {
          reindeer$flying <- FALSE
          reindeer$timeInMode <- 0
        }
      } else {
        reindeer$timeInMode <- reindeer$timeInMode + 1
        if (reindeer$timeInMode == reindeer$restTime) {
          reindeer$flying <- TRUE
          reindeer$timeInMode <- 0
        }
      }
      reindeers[[j]] <- reindeer
    }
  }
  return(reindeers)
}

findMaxDistance <- function(reindeers) {
  maxDistance <- 0
  for (reindeer in reindeers) {
    if (reindeer$distance > maxDistance) {
      maxDistance <- reindeer$distance
    }
  }
  return(maxDistance)
}

reindeers <- readReindeerDetails("input.txt")
reindeers <- simulateRace(reindeers, 2503)
maxDistance <- findMaxDistance(reindeers)
print(maxDistance)