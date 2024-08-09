read_reindeer_details <- function(filename) {
  reindeers <- list()
  lines <- readLines(filename)
  
  for (line in lines) {
    parts <- unlist(strsplit(line, " "))
    speed <- as.integer(parts[4])
    fly_time <- as.integer(parts[7])
    rest_time <- as.integer(parts[14])
    reindeers <- append(reindeers, list(list(speed = speed, fly_time = fly_time, rest_time = rest_time, distance = 0, points = 0, flying = TRUE, time_in_mode = 0)))
  }
  
  return(reindeers)
}

simulate_race_with_points <- function(reindeers, total_seconds) {
  for (i in 1:total_seconds) {
    max_distance <- 0
    for (j in seq_along(reindeers)) {
      reindeer <- reindeers[[j]]
      if (reindeer$flying) reindeer$distance <- reindeer$distance + reindeer$speed
      reindeer$time_in_mode <- reindeer$time_in_mode + 1
      if (reindeer$flying && reindeer$time_in_mode == reindeer$fly_time || !reindeer$flying && reindeer$time_in_mode == reindeer$rest_time) {
        reindeer$flying <- !reindeer$flying
        reindeer$time_in_mode <- 0
      }
      reindeers[[j]] <- reindeer
      if (reindeer$distance > max_distance) max_distance <- reindeer$distance
    }
    for (j in seq_along(reindeers)) {
      if (reindeers[[j]]$distance == max_distance) reindeers[[j]]$points <- reindeers[[j]]$points + 1
    }
  }
  return(reindeers)
}

find_max_points <- function(reindeers) {
  max_points <- 0
  for (reindeer in reindeers) {
    if (reindeer$points > max_points) max_points <- reindeer$points
  }
  return(max_points)
}

main <- function() {
  reindeers <- read_reindeer_details("input.txt")
  reindeers <- simulate_race_with_points(reindeers, 2503)
  max_points <- find_max_points(reindeers)
  print(max_points)
}

main()