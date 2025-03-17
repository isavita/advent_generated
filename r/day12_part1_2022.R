
# Define the main function
main <- function() {
  # Read the heightmap from the input file
  heightmap <- readLines("input.txt")
  heightmap <- strsplit(heightmap, "")

  # Convert the heightmap to a matrix of integers
  grid <- do.call(rbind, lapply(heightmap, function(row) {
    sapply(row, function(char) {
      if (char == "S") {
        return(0)  # Start position, elevation 'a'
      } else if (char == "E") {
        return(25) # End position, elevation 'z'
      } else {
        return(match(char, letters) - 1)
      }
    })
  }))

    # Find the start and end positions
  start_pos <- which(sapply(heightmap, function(x) "S" %in% x))
  start_row <- (start_pos - 1) %% length(heightmap) + 1
  start_col <- which(heightmap[[start_row]] == "S")
  start <- c(start_row, start_col)

  end_pos <- which(sapply(heightmap, function(x) "E" %in% x))
  end_row <- (end_pos - 1) %% length(heightmap) + 1
  end_col <- which(heightmap[[end_row]] == "E")
  end <- c(end_row, end_col)

  # Initialize a queue for BFS
  queue <- list(list(pos = start, steps = 0))
  visited <- matrix(FALSE, nrow = nrow(grid), ncol = ncol(grid))
  visited[start[1], start[2]] <- TRUE

  # Define possible moves (up, down, left, right)
  moves <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))

  # Perform BFS
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]  # Dequeue

    # Check if we reached the end
    if (all(current$pos == end)) {
      cat("Fewest steps:", current$steps, "\n")
      return()
    }

    # Explore neighbors
    for (move in moves) {
      new_pos <- current$pos + move
      
      #Check boundaries
      if (new_pos[1] >= 1 && new_pos[1] <= nrow(grid) &&
          new_pos[2] >= 1 && new_pos[2] <= ncol(grid)) {
        
        #Check if visited and elevation difference
        if (!visited[new_pos[1], new_pos[2]] &&
            grid[new_pos[1], new_pos[2]] - grid[current$pos[1], current$pos[2]] <= 1) {
          
            queue <- c(queue, list(list(pos = new_pos, steps = current$steps + 1)))
            visited[new_pos[1], new_pos[2]] <- TRUE # Mark as visited
        }
      }
    }
  }

  cat("No path found.\n")
}


# Set the working directory to the script's location.  Important for CRAN checks.
tryCatch(
  {
      initial.options <- commandArgs(trailingOnly = FALSE)
      file.arg.name <- "--file="
      script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
      if (length(script.name) > 0) {
          setwd(dirname(script.name))
      }
  },
  error = function(e) {
    # Fallback in case of error (e.g., when running interactively)
    cat("Could not determine script directory, setting to current.\n")
  }
)
# Run the main function
main()
