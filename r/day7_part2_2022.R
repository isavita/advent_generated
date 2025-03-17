
# Define a function to build the directory tree
build_tree <- function(input_lines) {
  # Initialize the file system as a list
  fs <- list()
  fs[["/"]] <- 0  # Root directory
  path <- c("/")  # Start at the root directory
  
  for (line in input_lines) {
    parts <- strsplit(line, " ")[[1]]
    
    if (parts[1] == "$") {  # Command
      if (parts[2] == "cd") {
        if (parts[3] == "..") {
          # Move up one level
          path <- head(path, -1)
        } else if (parts[3] == "/") {
            # Go to root
            path <- c("/")
        }
        
        else {
          # Move to a subdirectory
          dir_name <- parts[3]
          path <- c(path, paste0(tail(path,1), if(tail(path,1)=="/") "" else "/", dir_name)) # Full path to handle the nested directories
          
        }
      }
    } else {  # ls output
      current_dir_full_path <- paste0(path, collapse = "")
      
      if (parts[1] == "dir") {
        dir_name <- parts[2]
        new_dir_full_path <- paste0(tail(path,1), if(tail(path,1)=="/") "" else "/", dir_name) # Store the full path
          
        if(is.null(fs[[new_dir_full_path]])){
            fs[[new_dir_full_path]] <- 0
        }
          
      } else {  # File
        file_size <- as.integer(parts[1])
        
        # Add the size to all directories in the current path.
        for (i in 1:length(path)){
            dir_to_update <- paste0(path[1:i], collapse = "")
            if (is.null(fs[[dir_to_update]])) {
              fs[[dir_to_update]] <- 0  # Initialize if necessary (should not happen with correct input)
            }
            fs[[dir_to_update]] <- fs[[dir_to_update]] + file_size

        }
      }
    }
  }
  return(fs)
}

# Calculate directory sizes (recursive function not needed as we store total sizes)
calculate_sizes <- function(fs) {
    return(fs)
}

# Function to solve Part One
solve_part_one <- function(dir_sizes) {
    sum(unlist(dir_sizes[unlist(dir_sizes) <= 100000]))
}

# Function to solve Part Two
solve_part_two <- function(dir_sizes) {
  total_space <- 70000000
  required_space <- 30000000
  used_space <- dir_sizes[["/"]]
  unused_space <- total_space - used_space
  space_to_free <- required_space - unused_space
  
  candidates <- dir_sizes[unlist(dir_sizes) >= space_to_free]
  return(min(unlist(candidates)))
}

# Main function to read input, process data, and print results
main <- function() {
  # Read input from file
  input_lines <- readLines("input.txt")
  
  # Build the directory tree and calculate sizes
  fs <- build_tree(input_lines)
  dir_sizes <- calculate_sizes(fs)
  
  # Solve and print Part One
  part_one_answer <- solve_part_one(dir_sizes)
  cat("Part One:", part_one_answer, "\n")
  
  # Solve and print Part Two
  part_two_answer <- solve_part_two(dir_sizes)
  cat("Part Two:", part_two_answer, "\n")
}

# Run the main function
main()

