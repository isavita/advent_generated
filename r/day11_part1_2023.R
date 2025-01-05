
read_file <- function(filename) {
  readLines(filename)
}

build_grid <- function(input, empty = ".") {
  width <- nchar(input[1])
  height <- length(input)
  data <- list()
  
  for (y in 1:height) {
    line <- strsplit(input[y], "")[[1]]
    for (x in 1:width) {
      if (line[x] != empty) {
        data[[paste(x, y)]] <- line[x]
      }
    }
  }
  
  list(width = width, height = height, data = data)
}

get_empty_rows <- function(grid) {
  empty_rows <- integer()
  for (y in 1:grid$height) {
    is_empty <- TRUE
    for (x in 1:grid$width) {
      if (!is.null(grid$data[[paste(x, y)]])) {
        is_empty <- FALSE
        break
      }
    }
    if (is_empty) {
      empty_rows <- c(empty_rows, y)
    }
  }
  empty_rows
}

get_empty_cols <- function(grid) {
  empty_cols <- integer()
  for (x in 1:grid$width) {
    is_empty <- TRUE
    for (y in 1:grid$height) {
      if (!is.null(grid$data[[paste(x, y)]])) {
        is_empty <- FALSE
        break
      }
    }
    if (is_empty) {
      empty_cols <- c(empty_cols, x)
    }
  }
  empty_cols
}

calculate_offsets <- function(empty_indexes, bound) {
  offsets <- rep(0, bound)
  for (idx in empty_indexes) {
    offsets[(idx + 1):bound] <- offsets[(idx + 1):bound] + 1
  }
  offsets
}

expand_grid <- function(grid, expansion_factor) {
  empty_cols <- get_empty_cols(grid)
  empty_rows <- get_empty_rows(grid)
  num_lines_to_add <- expansion_factor - 1
  
  new_width <- grid$width + length(empty_cols) * num_lines_to_add
  new_height <- grid$height + length(empty_rows) * num_lines_to_add
  new_data <- list()
  
  dxs <- calculate_offsets(empty_cols, grid$width)
  dys <- calculate_offsets(empty_rows, grid$height)
  
  for (y in 1:grid$height) {
    for (x in 1:grid$width) {
      if (!is.null(grid$data[[paste(x, y)]])) {
        new_x <- x + dxs[x] * num_lines_to_add
        new_y <- y + dys[y] * num_lines_to_add
        new_data[[paste(new_x, new_y)]] <- grid$data[[paste(x, y)]]
      }
    }
  }
  
  list(width = new_width, height = new_height, data = new_data)
}

calculate_length <- function(c1, c2) {
  abs(c2[1] - c1[1]) + abs(c2[2] - c1[2])
}

solve <- function(input) {
  grid <- build_grid(input)
  expanded_grid <- expand_grid(grid, 2)
  
  coords <- names(expanded_grid$data)
  n <- length(coords)
  res <- 0
  
  if (n > 1) {
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        coord1 <- as.integer(strsplit(coords[i], " ")[[1]])
        coord2 <- as.integer(strsplit(coords[j], " ")[[1]])
        res <- res + calculate_length(coord1, coord2)
      }
    }
  }
  
  res
}

input <- read_file("input.txt")
result <- solve(input)
cat(result, "\n")
