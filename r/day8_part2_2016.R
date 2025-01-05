
# Function to initialize the screen
initialize_screen <- function(width, height) {
  matrix(0, nrow = height, ncol = width)
}

# Function to apply the rect operation
apply_rect <- function(screen, a, b) {
  screen[1:b, 1:a] <- 1
  screen
}

# Function to apply the rotate row operation
apply_rotate_row <- function(screen, row, by) {
  screen[row + 1, ] <- c(tail(screen[row + 1, ], by), head(screen[row + 1, ], ncol(screen) - by))
  screen
}

# Function to apply the rotate column operation
apply_rotate_col <- function(screen, col, by) {
  screen[, col + 1] <- c(tail(screen[, col + 1], by), head(screen[, col + 1], nrow(screen) - by))
  screen
}

# Function to process a single instruction
process_instruction <- function(screen, instruction) {
  parts <- strsplit(instruction, " ")[[1]]
  if (parts[1] == "rect") {
    dims <- as.integer(strsplit(parts[2], "x")[[1]])
    screen <- apply_rect(screen, dims[1], dims[2])
  } else if (parts[1] == "rotate") {
    if (parts[2] == "row") {
      row <- as.integer(gsub("y=", "", parts[3]))
      by <- as.integer(parts[5])
      screen <- apply_rotate_row(screen, row, by)
    } else if (parts[2] == "column") {
      col <- as.integer(gsub("x=", "", parts[3]))
      by <- as.integer(parts[5])
      screen <- apply_rotate_col(screen, col, by)
    }
  }
  screen
}

# Function to count lit pixels
count_lit_pixels <- function(screen) {
  sum(screen)
}

# Function to print the screen
print_screen <- function(screen) {
  for (i in 1:nrow(screen)) {
    cat(paste(ifelse(screen[i, ] == 1, "#", "."), collapse = ""), "\n")
  }
}

# Main program
main <- function() {
  screen_width <- 50
  screen_height <- 6
  screen <- initialize_screen(screen_width, screen_height)
  
  instructions <- readLines("input.txt")
  
  for (instruction in instructions) {
    screen <- process_instruction(screen, instruction)
  }
  
  lit_pixels <- count_lit_pixels(screen)
  cat("Number of lit pixels:", lit_pixels, "\n")
  
  cat("Screen output:\n")
  print_screen(screen)
}

# Run the main program
main()
