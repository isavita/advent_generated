
look_and_say <- function(input_str) {
  result <- ""
  i <- 1
  while (i <= nchar(input_str)) {
    current_digit <- substr(input_str, i, i)
    count <- 1
    j <- i + 1
    while (j <= nchar(input_str) && substr(input_str, j, j) == current_digit) {
      count <- count + 1
      j <- j + 1
    }
    result <- paste0(result, count, current_digit)
    i <- j
  }
  return(result)
}

# Read input from file
input_line <- readLines("input.txt", n = 1)

# Apply look-and-say 40 times
sequence <- input_line
for (i in 1:40) {
  sequence <- look_and_say(sequence)
}

# Print the length of the result
cat(nchar(sequence), "\n")
