move_tail <- function(head, tail) {
  if (abs(head[1] - tail[1]) <= 1 && abs(head[2] - tail[2]) <= 1) return(tail)
  tail[1] <- tail[1] + sign(head[1] - tail[1])
  tail[2] <- tail[2] + sign(head[2] - tail[2])
  return(tail)
}

simulate_rope <- function(motions) {
  knots <- matrix(0, nrow = 10, ncol = 2)
  tail_visited <- list()
  
  for (motion in motions) {
    direction <- substr(motion, 1, 1)
    steps <- as.integer(substr(motion, 3, nchar(motion)))
    
    for (step in 1:steps) {
      if (direction == "R") knots[1, 1] <- knots[1, 1] + 1
      if (direction == "L") knots[1, 1] <- knots[1, 1] - 1
      if (direction == "U") knots[1, 2] <- knots[1, 2] + 1
      if (direction == "D") knots[1, 2] <- knots[1, 2] - 1
      
      for (i in 2:10) {
        knots[i, ] <- move_tail(knots[i - 1, ], knots[i, ])
      }
      
      tail_visited[[length(tail_visited) + 1]] <- paste(knots[10, ], collapse = ",")
    }
  }
  
  return(length(unique(tail_visited)))
}

input_data <- readLines("input.txt")
result <- simulate_rope(input_data)
cat(result, "\n")