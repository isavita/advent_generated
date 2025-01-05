
# Function to read the input file and process the track and carts
process_input <- function(filename) {
  lines <- readLines(filename)
  track <- strsplit(lines, "")
  
  carts <- list()
  for (y in seq_along(track)) {
    for (x in seq_along(track[[y]])) {
      if (track[[y]][x] %in% c("^", "v", "<", ">")) {
        carts <- append(carts, list(list(x = x - 1, y = y - 1, dir = track[[y]][x], turn = 0)))
        if (track[[y]][x] == "^" || track[[y]][x] == "v") {
          track[[y]][x] <- "|"
        } else {
          track[[y]][x] <- "-"
        }
      }
    }
  }
  
  list(track = track, carts = carts)
}

# Function to move a cart
move_cart <- function(cart, track) {
  x <- cart$x
  y <- cart$y
  dir <- cart$dir
  turn <- cart$turn
  
  if (dir == "^") {
    y <- y - 1
  } else if (dir == "v") {
    y <- y + 1
  } else if (dir == "<") {
    x <- x - 1
  } else if (dir == ">") {
    x <- x + 1
  }
  
  new_dir <- dir
  
  track_char <- track[[y + 1]][x + 1]
  
  if (track_char == "/") {
    if (dir == "^") new_dir <- ">"
    else if (dir == "v") new_dir <- "<"
    else if (dir == "<") new_dir <- "v"
    else if (dir == ">") new_dir <- "^"
  } else if (track_char == "\\") {
    if (dir == "^") new_dir <- "<"
    else if (dir == "v") new_dir <- ">"
    else if (dir == "<") new_dir <- "^"
    else if (dir == ">") new_dir <- "v"
  } else if (track_char == "+") {
    if (turn %% 3 == 0) { # Left
      if (dir == "^") new_dir <- "<"
      else if (dir == "v") new_dir <- ">"
      else if (dir == "<") new_dir <- "v"
      else if (dir == ">") new_dir <- "^"
    } else if (turn %% 3 == 2) { # Right
      if (dir == "^") new_dir <- ">"
      else if (dir == "v") new_dir <- "<"
      else if (dir == "<") new_dir <- "^"
      else if (dir == ">") new_dir <- "v"
    }
    turn <- turn + 1
  }
  
  list(x = x, y = y, dir = new_dir, turn = turn)
}

# Function to check for collisions
check_collisions <- function(carts) {
  coords <- matrix(unlist(lapply(carts, function(cart) c(cart$x, cart$y))), ncol = 2, byrow = TRUE)
  
  if (nrow(unique(coords)) < nrow(coords)) {
    dupes <- coords[duplicated(coords), , drop = FALSE]
    return(list(collision = TRUE, x = dupes[1,1], y = dupes[1,2]))
  }
  
  list(collision = FALSE)
}

# Main function
main <- function() {
  input_data <- process_input("input.txt")
  track <- input_data$track
  carts <- input_data$carts
  
  while (TRUE) {
    # Sort carts by row and then column
    carts <- carts[order(sapply(carts, function(cart) cart$y), sapply(carts, function(cart) cart$x))]
    
    for (i in seq_along(carts)) {
      carts[[i]] <- move_cart(carts[[i]], track)
      collision_check <- check_collisions(carts)
      if (collision_check$collision) {
        cat(paste(collision_check$x, ",", collision_check$y, "\n", sep = ""))
        return()
      }
    }
  }
}

# Run the main function
main()
