
calculate_power_level <- function(x, y, serial_number) {
  rack_id <- x + 10
  power_level <- rack_id * y
  power_level <- power_level + serial_number
  power_level <- power_level * rack_id
  power_level <- (power_level %/% 100) %% 10
  power_level <- power_level - 5
  return(power_level)
}

calculate_cumulative_grid <- function(grid) {
    n <- nrow(grid)
    cumulative_grid <- matrix(0, nrow = n + 1, ncol = n + 1)
    for (x in 2:(n + 1)) {
        for (y in 2:(n + 1)) {
            cumulative_grid[x, y] <- (
                grid[x - 1, y - 1]
                + cumulative_grid[x - 1, y]
                + cumulative_grid[x, y - 1]
                - cumulative_grid[x - 1, y - 1]
            )
        }
    }
    return(cumulative_grid)
}

calculate_total_power <- function(cumulative_grid, x, y, size) {
  x1 <- x
  y1 <- y
  x2 <- x + size -1
  y2 <- y + size -1
  total_power <- (
    cumulative_grid[x2 + 1, y2 + 1]
    - cumulative_grid[x1, y2 + 1]
    - cumulative_grid[x2 + 1, y1]
    + cumulative_grid[x1, y1]
  )
  return(total_power)
}

find_largest_square <- function(cumulative_grid) {
  max_power <- -Inf
  max_coordinates <- NULL
  for (size in 1:300) {
    for (x in 1:(301 - size)) {
      for (y in 1:(301 - size)) {
        total_power <- calculate_total_power(cumulative_grid, x, y, size)
        if (total_power > max_power) {
          max_power <- total_power
          max_coordinates <- c(x, y, size)
        }
      }
    }
  }
  return(max_coordinates)
}

main <- function() {
  serial_number <- as.integer(readLines("input.txt"))
  grid <- matrix(0, nrow = 300, ncol = 300)
  for (x in 1:300) {
    for (y in 1:300) {
      grid[x, y] <- calculate_power_level(x, y, serial_number)
    }
  }

  cumulative_grid <- calculate_cumulative_grid(grid)
  max_coordinates <- find_largest_square(cumulative_grid)

  cat(sprintf("%d,%d,%d", max_coordinates[1], max_coordinates[2], max_coordinates[3]))
}

main()
