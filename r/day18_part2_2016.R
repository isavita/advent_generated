
# Function to determine the next row based on the previous row
next_row <- function(prev_row) {
  len <- nchar(prev_row)
  next_r <- character(len)
  
  # Pad the previous row with safe tiles on both ends
  padded_row <- paste0(".", prev_row, ".")
  
  for (i in 1:len) {
    left <- substr(padded_row, i, i)
    center <- substr(padded_row, i + 1, i + 1)
    right <- substr(padded_row, i + 2, i + 2)
    
    # Determine if the current tile is a trap or safe
    if ((left == "^" && center == "^" && right == ".") ||
        (center == "^" && right == "^" && left == ".") ||
        (left == "^" && center == "." && right == ".") ||
        (left == "." && center == "." && right == "^")) {
      next_r[i] <- "^"
    } else {
      next_r[i] <- "."
    }
  }
  
  return(paste(next_r, collapse = ""))
}

# Main function
main <- function() {
  # Read the first row from the input file
  con <- file("input.txt", "r")
  first_row <- readLines(con, n = 1)
  close(con)

  # Part 1: Calculate safe tiles for 40 rows
  rows <- first_row
  safe_count_40 <- 0
  for (i in 1:40) {
    safe_count_40 <- safe_count_40 + sum(strsplit(rows, "")[[1]] == ".")
    rows <- next_row(rows)
  }
  cat("Part 1: Safe tiles in 40 rows:", safe_count_40, "\n")
  
  # Part 2: Calculate safe tiles for 400000 rows
   rows <- first_row
    safe_count_400000 <- 0
    for (i in 1:400000){
        safe_count_400000 <- safe_count_400000 + sum(strsplit(rows, "")[[1]] == ".")
        rows <- next_row(rows)
    }
   cat("Part 2: Safe tiles in 400000 rows:", safe_count_400000, "\n")
}

# Run the main function
main()
