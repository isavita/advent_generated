
calculate_power_level <- function(x, y, serial_number) {
  rack_id <- x + 10
  power_level <- rack_id * y
  power_level <- power_level + serial_number
  power_level <- power_level * rack_id
  hundreds_digit <- (power_level %/% 100) %% 10
  power_level <- hundreds_digit - 5
  return(power_level)
}

find_max_power_square <- function(serial_number) {
  grid <- matrix(0, nrow = 300, ncol = 300)
  for (x in 1:300) {
    for (y in 1:300) {
      grid[x, y] <- calculate_power_level(x, y, serial_number)
    }
  }
  
  max_power <- -Inf
  max_x <- -1
  max_y <- -1
  
  for (x in 1:298) {
    for (y in 1:298) {
      current_power <- sum(grid[x:(x+2), y:(y+2)])
      if (current_power > max_power) {
        max_power <- current_power
        max_x <- x
        max_y <- y
      }
    }
  }
  
  return(c(max_x, max_y))
}

# Read input from file
input <- readLines("input.txt")
serial_number <- as.integer(input[1])

# Find the coordinates of the top-left cell of the square with the largest total power
result <- find_max_power_square(serial_number)

# Print the result to standard output
cat(paste(result[1], ",", result[2], "\n", sep=""))
