rotate_waypoint <- function(wx, wy, angle) {
  rad <- angle * (pi / 180)
  cos_angle <- cos(rad)
  sin_angle <- sin(rad)
  new_wx <- round(cos_angle * wx - sin_angle * wy)
  new_wy <- round(sin_angle * wx + cos_angle * wy)
  return(c(new_wx, new_wy))
}

navigate <- function(instructions) {
  ship_pos <- c(0, 0)
  waypoint <- c(10, 1)
  
  for (instruction in instructions) {
    action <- substr(instruction, 1, 1)
    value <- as.numeric(substr(instruction, 2, nchar(instruction)))
    
    switch(action,
           'N' = waypoint[2] <- waypoint[2] + value,
           'S' = waypoint[2] <- waypoint[2] - value,
           'E' = waypoint[1] <- waypoint[1] + value,
           'W' = waypoint[1] <- waypoint[1] - value,
           'L' = {
             waypoint <- rotate_waypoint(waypoint[1], waypoint[2], value)
           },
           'R' = {
             waypoint <- rotate_waypoint(waypoint[1], waypoint[2], -value)
           },
           'F' = {
             ship_pos[1] <- ship_pos[1] + waypoint[1] * value
             ship_pos[2] <- ship_pos[2] + waypoint[2] * value
           }
    )
  }
  
  return(sum(abs(ship_pos)))
}

main <- function() {
  instructions <- readLines("input.txt")
  manhattan_distance <- navigate(instructions)
  cat("Manhattan Distance:", manhattan_distance, "\n")
}

main()