
input <- readLines("input.txt")

get_angle <- function(x1, y1, x2, y2) {
  return(atan2(y2 - y1, x2 - x1) * 180 / pi)
}

get_distance <- function(x1, y1, x2, y2) {
  return(sqrt((x2 - x1)^2 + (y2 - y1)^2))
}

asteroids <- data.frame(x = integer(), y = integer(), visible = integer())

for (i in 1:length(input)) {
  for (j in 1:nchar(input[[i]])) {
    if (substr(input[[i]], j, j) == "#") {
      asteroids <- rbind(asteroids, data.frame(x = j - 1, y = i - 1, visible = 0))
    }
  }
}

for (i in 1:nrow(asteroids)) {
  angles <- c()
  for (j in 1:nrow(asteroids)) {
    if (i != j) {
      angle <- get_angle(asteroids[i, "x"], asteroids[i, "y"], asteroids[j, "x"], asteroids[j, "y"])
      if (!(angle %in% angles)) {
        angles <- c(angles, angle)
        asteroids[i, "visible"] <- asteroids[i, "visible"] + 1
      }
    }
  }
}

result <- max(asteroids$visible)
print(result)
