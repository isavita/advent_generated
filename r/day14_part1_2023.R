
input <- readLines("input.txt")
grid <- strsplit(input, "")
rows <- length(grid)
cols <- length(grid[[1]])

shift_rocks <- function(grid, dir) {
  if (dir[1] == 0 && dir[2] == -1) {
    for (x in 1:cols) {
      for (y in 1:rows) {
        if (grid[[y]][x] == "O") {
          current_y <- y
          before_y <- y + dir[2]
          while (before_y >= 1 && before_y <= rows && grid[[before_y]][x] == ".") {
            grid[[before_y]][x] <- "O"
            grid[[current_y]][x] <- "."
            current_y <- before_y
            before_y <- before_y + dir[2]
          }
        }
      }
    }
  } else if (dir[1] == -1 && dir[2] == 0) {
    for (y in 1:rows) {
      for (x in 1:cols) {
        if (grid[[y]][x] == "O") {
          current_x <- x
          before_x <- x + dir[1]
          while (before_x >= 1 && before_x <= cols && grid[[y]][before_x] == ".") {
            grid[[y]][before_x] <- "O"
            grid[[y]][current_x] <- "."
            current_x <- before_x
            before_x <- before_x + dir[1]
          }
        }
      }
    }
  } else if (dir[1] == 0 && dir[2] == 1) {
    for (x in 1:cols) {
      for (y in rows:1) {
        if (grid[[y]][x] == "O") {
          current_y <- y
          before_y <- y + dir[2]
          while (before_y >= 1 && before_y <= rows && grid[[before_y]][x] == ".") {
            grid[[before_y]][x] <- "O"
            grid[[current_y]][x] <- "."
            current_y <- before_y
            before_y <- before_y + dir[2]
          }
        }
      }
    }
  } else if (dir[1] == 1 && dir[2] == 0) {
    for (y in 1:rows) {
      for (x in cols:1) {
        if (grid[[y]][x] == "O") {
          current_x <- x
          before_x <- x + dir[1]
          while (before_x >= 1 && before_x <= cols && grid[[y]][before_x] == ".") {
            grid[[y]][before_x] <- "O"
            grid[[y]][current_x] <- "."
            current_x <- before_x
            before_x <- before_x + dir[1]
          }
        }
      }
    }
  }
  return(grid)
}

calculate_load <- function(grid) {
  load <- 0
  for (y in 1:rows) {
    for (x in 1:cols) {
      if (grid[[y]][x] == "O") {
        load <- load + (rows - y + 1)
      }
    }
  }
  return(load)
}

solve <- function(input) {
  grid <- strsplit(input, "")
  grid <- shift_rocks(grid, c(0, -1))
  calculate_load(grid)
}

result <- solve(input)
cat(result, "\n")
