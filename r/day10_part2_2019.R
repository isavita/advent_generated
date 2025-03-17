
# Function to calculate the greatest common divisor (GCD)
gcd <- function(a, b) {
  if (b == 0) a else gcd(b, a %% b)
}

# Function to calculate visible asteroids from a given location
calculate_visible <- function(map, row, col) {
    visible <- 0
    asteroids <- which(map == '#', arr.ind = TRUE)
    asteroids <- asteroids[!(asteroids[,1] == row & asteroids[,2] == col), , drop = FALSE]

    if (nrow(asteroids) == 0) {
        return(0)
    }
    
    directions <- matrix(0, nrow = nrow(asteroids), ncol = 2)
    directions[,1] <- asteroids[,1] - row
    directions[,2] <- asteroids[,2] - col

    unique_directions <- matrix(0, nrow = 0, ncol = 2)

    for (i in 1:nrow(directions)) {
      divisor <- gcd(abs(directions[i,1]), abs(directions[i,2]))
      reduced_direction <- directions[i,] / divisor
      
      is_unique <- TRUE
      if(nrow(unique_directions) > 0){
        for(j in 1:nrow(unique_directions)){
          if(all(unique_directions[j,] == reduced_direction)) {
            is_unique <- FALSE
            break
          }
        }
      }

      if(is_unique){
        unique_directions <- rbind(unique_directions, reduced_direction)
      }
    }
    
    visible <- nrow(unique_directions)
    return(visible)
}

# Function to find the best asteroid location
find_best_location <- function(map) {
    best_row <- -1
    best_col <- -1
    max_visible <- -1

    asteroids <- which(map == '#', arr.ind = TRUE)

    for (i in 1:nrow(asteroids)) {
        row <- asteroids[i, 1]
        col <- asteroids[i, 2]
        visible <- calculate_visible(map, row, col)

        if (visible > max_visible) {
            max_visible <- visible
            best_row <- row
            best_col <- col
        }
    }

    return(list(row = best_row, col = best_col, visible = max_visible))
}


# Function to calculate angle between two points
calculate_angle <- function(station_row, station_col, asteroid_row, asteroid_col) {
  delta_x <- asteroid_col - station_col
  delta_y <- asteroid_row - station_row
  angle <- atan2(delta_x, -delta_y) # Negative delta_y because 0,0 is top-left
  if (angle < 0) {
    angle <- angle + 2 * pi
  }
  return(angle)
}

# Function to calculate Euclidean distance
calculate_distance <- function(x1, y1, x2, y2) {
  return(sqrt((x2 - x1)^2 + (y2 - y1)^2))
}

# Function to vaporize asteroids
vaporize_asteroids <- function(map, station_row, station_col) {
  asteroids <- which(map == '#', arr.ind = TRUE)
  asteroids <- asteroids[!(asteroids[, 1] == station_row & asteroids[, 2] == station_col), , drop = FALSE]

  asteroid_data <- data.frame(
    row = asteroids[, 1],
    col = asteroids[, 2],
    angle = numeric(nrow(asteroids)),
    distance = numeric(nrow(asteroids)),
    vaporized = logical(nrow(asteroids)),
    order = numeric(nrow(asteroids))
  )
  
  for (i in 1:nrow(asteroid_data)) {
    asteroid_data$angle[i] <- calculate_angle(station_row, station_col, asteroid_data$row[i], asteroid_data$col[i])
    asteroid_data$distance[i] <- calculate_distance(station_row, station_col, asteroid_data$row[i], asteroid_data$col[i])
  }
  
  vaporized_count <- 0
  current_angle <- -1  # Initialize to something impossible
  
  while (vaporized_count < nrow(asteroid_data)) {
    
    #find closest asteroid at each angle
    asteroid_data_not_vap <- asteroid_data[!asteroid_data$vaporized, , drop = FALSE]
    
    if(nrow(asteroid_data_not_vap) == 0) break
    
    #get unique angles
    angles <- unique(asteroid_data_not_vap$angle)
    angles <- angles[order(angles)]
    
    #get relevant asteroids
    relevant_asteroids = data.frame()
    for(current_a in angles){
      
      current_angle_asteroids <- asteroid_data_not_vap[asteroid_data_not_vap$angle == current_a, , drop = FALSE]
      min_dist = min(current_angle_asteroids$distance)
      relevant_asteroids = rbind(relevant_asteroids, current_angle_asteroids[current_angle_asteroids$distance == min_dist, , drop = FALSE])
    }
    
    #sort by angle
    relevant_asteroids = relevant_asteroids[order(relevant_asteroids$angle), , drop = FALSE]
      
    for (i in 1:nrow(relevant_asteroids)) {
      vaporized_count <- vaporized_count + 1
      asteroid_index <- which(asteroid_data$row == relevant_asteroids$row[i] & asteroid_data$col == relevant_asteroids$col[i])
      asteroid_data$vaporized[asteroid_index] <- TRUE
      asteroid_data$order[asteroid_index] <- vaporized_count
    }
  }
  
  return(asteroid_data)
}


# Main function
main <- function() {
    # Read input from file
    lines <- readLines("input.txt")
    map <- matrix(unlist(strsplit(lines, "")), nrow = length(lines), byrow = TRUE)
    
    # Part 1
    best_location <- find_best_location(map)
    cat("Part 1: Best location is (", best_location$col -1 , ",", best_location$row - 1, ") with", best_location$visible, "visible asteroids.\n")
    
    # Part 2
    vaporized <- vaporize_asteroids(map, best_location$row, best_location$col)
    asteroid_200 <- vaporized[vaporized$order == 200, ]
    result_part2 <- (asteroid_200$col - 1) * 100 + (asteroid_200$row - 1)
    cat("Part 2: The 200th asteroid to be vaporized is at (", asteroid_200$col - 1, ",", asteroid_200$row - 1, "). Result:", result_part2, "\n")
}

# Run the main function
main()
