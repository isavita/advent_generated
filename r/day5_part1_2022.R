
# Main entry point for the program.
main <- function() {
  # Reads the input from "input.txt".
  input <- readLines("input.txt")

  # Efficiently finds the line separating stacks from instructions.
  separator_line <- which(input == "")[1]

  # Parses the stack data, handling variable stack numbers efficiently.
  stacks_str <- input[1:(separator_line - 2)]
  stacks <- parse_stacks(stacks_str)

  # Parses and executes the move instructions.
  instructions_str <- input[(separator_line + 1):length(input)]
  stacks <- process_instructions(stacks, instructions_str)

  # Concisely extracts and prints the top crates.
  top_crates <- sapply(stacks, function(stack) ifelse(length(stack) > 0, stack[length(stack)], ""))
  cat(paste(top_crates, collapse = ""), "\n")
}


parse_stacks <- function(stacks_str) {
    #Determines the number of stacks dynamically.
    num_stacks <- (nchar(stacks_str[length(stacks_str)])+1) / 4
    stacks <- vector("list", num_stacks)
    
    for (i in rev(seq_along(stacks_str))) {
        line <- stacks_str[i]
        for (j in 1:num_stacks) {
            crate <- substr(line, (j - 1) * 4 + 2, (j - 1) * 4 + 2)
            if (crate != " ") {
                stacks[[j]] <- c(stacks[[j]], crate)
            }
        }
    }
    return(stacks)
}



process_instructions <- function(stacks, instructions_str) {
  for (instruction in instructions_str) {
    # Uses a regular expression for efficient instruction parsing.
    parts <- as.integer(regmatches(instruction, gregexpr("[0-9]+", instruction))[[1]])
    count <- parts[1]
    from <- parts[2]
    to <- parts[3]

    # Optimized crate movement: moves crates one by one.
    for (i in 1:count) {
      if (length(stacks[[from]]) > 0) {
        stacks[[to]] <- c(stacks[[to]], tail(stacks[[from]], 1))
        stacks[[from]] <- head(stacks[[from]], -1)
      }
    }
  }
  return(stacks)
}


# Calls the main function to start execution.
main()
