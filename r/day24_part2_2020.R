
# Function to parse directions and find the final tile coordinates
get_tile_coordinates <- function(directions) {
  x <- 0
  y <- 0
  i <- 1
  while (i <= nchar(directions)) {
    if (substr(directions, i, i) == "e") {
      x <- x + 2
      i <- i + 1
    } else if (substr(directions, i, i) == "w") {
      x <- x - 2
      i <- i + 1
    } else if (substr(directions, i, i + 1) == "se") {
      x <- x + 1
      y <- y - 1
      i <- i + 2
    } else if (substr(directions, i, i + 1) == "sw") {
      x <- x - 1
      y <- y - 1
      i <- i + 2
    } else if (substr(directions, i, i + 1) == "ne") {
      x <- x + 1
      y <- y + 1
      i <- i + 2
    } else if (substr(directions, i, i + 1) == "nw") {
      x <- x - 1
      y <- y + 1
      i <- i + 2
    } else {
      stop("Invalid direction")
    }
  }
  return(c(x, y))
}

# Function to count black tiles after initial flips (Part 1)
count_black_tiles <- function(lines) {
    tiles <- list()
    for (line in lines) {
        coords <- get_tile_coordinates(line)
        coord_str <- paste(coords[1], coords[2], sep=",")

        if (is.null(tiles[[coord_str]])) {
          tiles[[coord_str]] <- TRUE  # Initially white, flip to black
        } else {
          tiles[[coord_str]] <- !tiles[[coord_str]] # Flip
        }
    }
    
    black_count <- 0
    for(tile_state in tiles){
      if(tile_state){
        black_count <- black_count + 1
      }
    }
    return(black_count)
}

# Function to get neighbors of a tile
get_neighbors <- function(x, y) {
    return(list(
        c(x + 2, y),
        c(x - 2, y),
        c(x + 1, y - 1),
        c(x - 1, y - 1),
        c(x + 1, y + 1),
        c(x - 1, y + 1)
    ))
}

# Function to simulate days (Part 2)
simulate_days <- function(initial_tiles, days) {
  
  tiles <- initial_tiles
  
  for (day in 1:days) {
    new_tiles <- list()
    
    # Collect all tiles to consider (black tiles and their neighbors)
    tiles_to_check <- list()
    for(coord_str in names(tiles)){
      if(tiles[[coord_str]]){
        coords <- as.integer(unlist(strsplit(coord_str, ",")))
        tiles_to_check[[coord_str]] <- TRUE #Add the black tile
        neighbors <- get_neighbors(coords[1], coords[2])
        for(neighbor in neighbors){
          neighbor_str <- paste(neighbor[1], neighbor[2], sep=",")
          tiles_to_check[[neighbor_str]] <- TRUE  #Add neighbors even if they are white at this stage.
        }
      }
    }
    
    
    for (coord_str in names(tiles_to_check)) {
      coords <- as.integer(unlist(strsplit(coord_str, ",")))
      neighbors <- get_neighbors(coords[1], coords[2])
      black_neighbors <- 0
      
      for (neighbor in neighbors) {
        neighbor_str <- paste(neighbor[1], neighbor[2], sep=",")
        if (!is.null(tiles[[neighbor_str]]) && tiles[[neighbor_str]]) {
          black_neighbors <- black_neighbors + 1
        }
      }

      current_state <- !is.null(tiles[[coord_str]]) && tiles[[coord_str]]

      if (current_state) { # Currently Black
        if (black_neighbors == 0 || black_neighbors > 2) {
          new_tiles[[coord_str]] <- FALSE # Flip to White
        } else{
          new_tiles[[coord_str]] <- TRUE # Keep Black
        }
      } else { # Currently White
        if (black_neighbors == 2) {
          new_tiles[[coord_str]] <- TRUE # Flip to Black
        } else {
          new_tiles[[coord_str]] <- FALSE # Keep white.
        }
      }
    }
      
    #Keep only black tiles
    tiles <- list()
    for(coord_str in names(new_tiles)){
      if(new_tiles[[coord_str]]){
        tiles[[coord_str]] <- TRUE
      }
    }

  }
  
  black_count <- 0
  for(tile_state in tiles){
    if(tile_state){
      black_count <- black_count + 1
    }
  }
    
  return(black_count)
}


# Main function
main <- function() {
  # Read input from file
  file_path <- "input.txt"
  lines <- readLines(file_path)

  # Part 1
  part1_result <- count_black_tiles(lines)
  cat("Part 1:", part1_result, "\n")

  # Part 2: Prepare initial tiles for simulation
  initial_tiles <- list()
  for (line in lines) {
      coords <- get_tile_coordinates(line)
      coord_str <- paste(coords[1], coords[2], sep=",")
      if (is.null(initial_tiles[[coord_str]])) {
          initial_tiles[[coord_str]] <- TRUE
      } else {
          initial_tiles[[coord_str]] <- !initial_tiles[[coord_str]]
      }
  }
    
  # Keep only initially black tiles
  tiles_part2 <- list()
  for(coord_str in names(initial_tiles)){
      if(initial_tiles[[coord_str]]){
        tiles_part2[[coord_str]] <- TRUE
      }
  }

  part2_result <- simulate_days(tiles_part2, 100)
  cat("Part 2:", part2_result, "\n")
}

# Run the main function
main()
