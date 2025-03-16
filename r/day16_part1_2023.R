
# Helper function to simulate the beam's path
simulate_beam <- function(grid, start_row, start_col, start_dir) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  energized <- matrix(FALSE, rows, cols)
  visited <- list()

  beams <- data.frame(row = start_row, col = start_col, dir = start_dir)

  while (nrow(beams) > 0) {
    new_beams <- data.frame(row = integer(), col = integer(), dir = character())

    for (i in 1:nrow(beams)) {
      row <- beams$row[i]
      col <- beams$col[i]
      dir <- beams$dir[i]

      if (row < 1 || row > rows || col < 1 || col > cols) {
        next  # Beam out of bounds
      }

      state <- paste(row, col, dir, sep = "_")
      if (state %in% visited) {
          next;
      }
      visited[[length(visited) + 1]] <- state

      energized[row, col] <- TRUE

      char <- grid[row, col]

      if (char == ".") {
        # Continue in the same direction
        if (dir == "right") new_beams <- rbind(new_beams, data.frame(row = row, col = col + 1, dir = "right"))
        if (dir == "left")  new_beams <- rbind(new_beams, data.frame(row = row, col = col - 1, dir = "left"))
        if (dir == "up")    new_beams <- rbind(new_beams, data.frame(row = row - 1, col = col, dir = "up"))
        if (dir == "down")  new_beams <- rbind(new_beams, data.frame(row = row + 1, col = col, dir = "down"))
      } else if (char == "/") {
        if (dir == "right") new_beams <- rbind(new_beams, data.frame(row = row - 1, col = col, dir = "up"))
        if (dir == "left")  new_beams <- rbind(new_beams, data.frame(row = row + 1, col = col, dir = "down"))
        if (dir == "up")    new_beams <- rbind(new_beams, data.frame(row = row, col = col + 1, dir = "right"))
        if (dir == "down")  new_beams <- rbind(new_beams, data.frame(row = row, col = col - 1, dir = "left"))
      } else if (char == "\\") {
        if (dir == "right") new_beams <- rbind(new_beams, data.frame(row = row + 1, col = col, dir = "down"))
        if (dir == "left")  new_beams <- rbind(new_beams, data.frame(row = row - 1, col = col, dir = "up"))
        if (dir == "up")    new_beams <- rbind(new_beams, data.frame(row = row, col = col - 1, dir = "left"))
        if (dir == "down")  new_beams <- rbind(new_beams, data.frame(row = row, col = col + 1, dir = "right"))
      } else if (char == "|") {
        if (dir == "right" || dir == "left") {
          new_beams <- rbind(new_beams, data.frame(row = row - 1, col = col, dir = "up"))
          new_beams <- rbind(new_beams, data.frame(row = row + 1, col = col, dir = "down"))
        } else {
          if (dir == "up")    new_beams <- rbind(new_beams, data.frame(row = row - 1, col = col, dir = "up"))
          if (dir == "down")  new_beams <- rbind(new_beams, data.frame(row = row + 1, col = col, dir = "down"))
        }
      } else if (char == "-") {
        if (dir == "up" || dir == "down") {
          new_beams <- rbind(new_beams, data.frame(row = row, col = col - 1, dir = "left"))
          new_beams <- rbind(new_beams, data.frame(row = row, col = col + 1, dir = "right"))
        } else {
          if (dir == "right") new_beams <- rbind(new_beams, data.frame(row = row, col = col + 1, dir = "right"))
          if (dir == "left")  new_beams <- rbind(new_beams, data.frame(row = row, col = col - 1, dir = "left"))
        }
      }
    }
    beams <- new_beams
  }
  return(sum(energized))
}

main <- function() {
  # Read the input grid from the file
  input_lines <- readLines("input.txt")
  grid <- do.call(rbind, strsplit(input_lines, ""))
  
  # Simulate the beam starting from the top-left corner, heading right
  energized_count <- simulate_beam(grid, 1, 1, "right")
  
  # Print the result
  cat(energized_count, "\n")
}

main()
