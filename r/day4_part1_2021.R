
# Function to check if a board has won
check_winner <- function(board) {
  for (i in 1:5) {
    if (all(board[i, ] == -1) || all(board[, i] == -1)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Function to calculate the score of a winning board
calculate_score <- function(board, last_number) {
  sum_unmarked <- sum(board[board != -1])
  return(sum_unmarked * last_number)
}

# Main function
main <- function() {
  # Read the input from the file
  input <- readLines("input.txt")
  
  # Extract the drawn numbers
  drawn_numbers <- as.integer(unlist(strsplit(input[1], ",")))
  
  # Extract the boards
  boards_data <- input[-(1:2)]
  boards_data <- boards_data[boards_data != ""]
  boards <- list()
  num_boards <- length(boards_data) / 5
  
  for (i in 1:num_boards) {
    start_row <- (i - 1) * 5 + 1
    end_row <- i * 5
    board_str <- boards_data[start_row:end_row]
    board <- matrix(as.integer(unlist(strsplit(gsub("^\\s+|\\s+$", "", paste(board_str,collapse=" ")), "\\s+"))), nrow = 5, byrow = TRUE)
    boards[[i]] <- board
  }

  # Play Bingo
  for (number in drawn_numbers) {
    for (i in seq_along(boards)) {
      boards[[i]][boards[[i]] == number] <- -1  # Mark the number
      
      if (check_winner(boards[[i]])) {
        final_score <- calculate_score(boards[[i]], number)
        cat(final_score, "\n")
        return() # Stop after first winner
      }
    }
  }
}

# Set working directory to script's location (only works in RStudio)
# if running interactively otherwise set it correctly
if (interactive()) {
    current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
    setwd(current_path)
}

# Run the main function
main()
