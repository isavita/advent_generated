
# Function to parse the input file
parse_input <- function(file_path) {
    lines <- readLines(file_path)
    
    # Find the blank line separating the map and the instructions
    blank_line_index <- which(lines == "")
    
    # Extract the map
    map_lines <- lines[1:(blank_line_index - 1)]
    max_width <- max(nchar(map_lines))
    map <- lapply(map_lines, function(line) {
        padded_line <- str_pad(line, max_width, side = "right", pad = " ")
        strsplit(padded_line, "")[[1]]
    })
    
    # Extract the instructions
    instructions_str <- lines[blank_line_index + 1]
    instructions <- strsplit(instructions_str, "")[[1]]
    
    return(list(map = map, instructions = instructions))
}

# Function to find the starting position
find_start <- function(map) {
    row <- 1
    col <- which(map[[1]] == ".")[1]
    return(c(row, col))
}

# Helper function for padding
str_pad <- function(string, width, side = "left", pad = " ") {
    len <- nchar(string)
    if (len >= width) {
        return(string)
    }
    
    padding_len <- width - len
    if (side == "left") {
        return(paste0(paste(rep(pad, padding_len), collapse = ""), string))
    } else if (side == "right") {
        return(paste0(string, paste(rep(pad, padding_len), collapse = "")))
    } else {
        stop("Invalid side. Choose 'left' or 'right'.")
    }
}
# Function to move and wrap around
move <- function(map, pos, dir, steps) {
    row <- pos[1]
    col <- pos[2]
    
    rows <- length(map)
    cols <- length(map[[1]])

    for (i in 1:steps) {
      next_row <- row
      next_col <- col
      
      if(dir == 0) { # right
          next_col <- (next_col) %% (cols) + 1
          while(map[[next_row]][next_col] == " ") {
              next_col <- (next_col) %% (cols) + 1
          }
          if(map[[next_row]][next_col] == "#") {
              break
          }
          col <- next_col
      }
        
      if(dir == 1) { #down
        next_row <- (next_row) %% (rows) + 1
        while(map[[next_row]][next_col] == " ") {
          next_row <- (next_row) %% (rows) + 1
        }
        if(map[[next_row]][next_col] == "#") {
          break
        }
        row <- next_row
      }
        
      if(dir == 2) { # left
          next_col <- (next_col - 2 + cols) %% (cols) + 1
          while(map[[next_row]][next_col] == " ") {
            next_col <- (next_col - 2 + cols) %% (cols) + 1
          }
          
          if(map[[next_row]][next_col] == "#") {
            break
          }
          col <- next_col
      }
        
      if(dir == 3) { # up
          next_row <- (next_row - 2 + rows) %% (rows) + 1
          
          while(map[[next_row]][next_col] == " ") {
            next_row <- (next_row - 2 + rows) %% (rows) + 1
          }
          
          if(map[[next_row]][next_col] == "#") {
             break
          }
          row <- next_row
      }

    }
    return(c(row, col))
}


# Function to calculate the final password
calculate_password <- function(map, instructions) {
    pos <- find_start(map)
    dir <- 0  # 0: right, 1: down, 2: left, 3: up
    
    num_buffer <- ""
    for (instr in instructions) {
      
        if (instr %in% c("R", "L")) {
            if (num_buffer != "") {
                steps <- as.integer(num_buffer)
                pos <- move(map, pos, dir, steps)
                num_buffer <- ""
            }

            if (instr == "R") {
                dir <- (dir + 1) %% 4
            } else {
                dir <- (dir - 1 + 4) %% 4
            }
        } else {
            num_buffer <- paste0(num_buffer, instr)
        }
    }
  
    # Handle remaining steps
    if (num_buffer != "") {
      steps <- as.integer(num_buffer)
      pos <- move(map, pos, dir, steps)
    }

    final_row <- pos[1]
    final_col <- pos[2]
    final_facing <- dir
    
    password <- 1000 * final_row + 4 * final_col + final_facing
    return(password)
}

# Main function
main <- function() {
    input <- parse_input("input.txt")
    map <- input$map
    instructions <- input$instructions
    
    password <- calculate_password(map, instructions)
    cat(password, "\n")
}

# Run the main function
main()
