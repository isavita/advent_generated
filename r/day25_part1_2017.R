
# Function to parse a single state's rules from the input
parse_state <- function(state_lines) {
  state_name <- gsub("In state ([A-Z]):", "\\1", state_lines[1])
  
  rules <- list()
  
  # Parse rules for current value 0
  rules[[1]] <- list(
    write = as.integer(gsub("    - Write the value ([0-1])\\.", "\\1", state_lines[3])),
    move = ifelse(grepl("right", state_lines[4]), 1, -1),
    next_state = gsub("    - Continue with state ([A-Z])\\.", "\\1", state_lines[5])
  )
  
  # Parse rules for current value 1
  rules[[2]] <- list(
    write = as.integer(gsub("    - Write the value ([0-1])\\.", "\\1", state_lines[7])),
    move = ifelse(grepl("right", state_lines[8]), 1, -1),
    next_state = gsub("    - Continue with state ([A-Z])\\.", "\\1", state_lines[9])
  )
  
  return(list(name = state_name, rules = rules))
}


# Main function to solve the puzzle
solve_day25 <- function(file_path) {
  
  # Read the input file
  lines <- readLines(file_path)
  
  # Extract initial state and checksum steps
  initial_state <- gsub("Begin in state ([A-Z])\\.", "\\1", lines[1])
  checksum_steps <- as.integer(gsub("Perform a diagnostic checksum after ([0-9]+) steps\\.", "\\1", lines[2]))
  
  # Parse states
  states_lines <- split(lines[4:length(lines)], cumsum(grepl("^In state", lines[4:length(lines)])))
  states <- lapply(states_lines, parse_state)
  names(states) <- sapply(states, function(x) x$name)
  states <- lapply(states, function(x) x$rules)  # Keep only the rules
  
  # Initialize tape and cursor
  tape <- integer(100000) # Start with a large tape; dynamically expand if needed.
  cursor <- 50000 # Cursor starts in the middle
  current_state <- initial_state
  
  # Run the Turing machine
  for (step in 1:checksum_steps) {
      
    current_value <- tape[cursor] + 1  # Adjust index for R (1-based)
    
    rule <- states[[current_state]][[current_value]]
    tape[cursor] <- rule$write
    cursor <- cursor + rule$move
    current_state <- rule$next_state

    # Dynamically expand tape if necessary (handles edge cases cleanly)
    if (cursor < 1) {
      tape <- c(integer(50000), tape)
      cursor <- cursor + 50000
    } else if (cursor > length(tape)) {
      tape <- c(tape, integer(50000))
    }
  }
  
  # Calculate diagnostic checksum
  checksum <- sum(tape)
  return(checksum)
}


# Main entry point
main <- function() {
  checksum <- solve_day25("input.txt")
  cat("Diagnostic Checksum:", checksum, "\n")
}

# Run the main function
main()

