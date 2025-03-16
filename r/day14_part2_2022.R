
parse_input <- function(input_str) {
  coord_sets <- strsplit(input_str, "\n")[[1]]
  coord_sets <- lapply(coord_sets, function(x) strsplit(x, " -> ")[[1]])
  coord_sets <- lapply(coord_sets, function(set) {
    lapply(set, function(coord) {
      as.integer(strsplit(coord, ",")[[1]])
    })
  })
  
  lowest_col <- Inf
  highest_row <- 0
  for (set in coord_sets) {
    for (coord in set) {
      lowest_col <- min(lowest_col, coord[[1]])
      highest_row <- max(highest_row, coord[[2]])
    }
  }
  
  extra_left_space <- 200
  highest_col <- 0
  for (i in seq_along(coord_sets)) {
    for (j in seq_along(coord_sets[[i]])) {
      coord_sets[[i]][[j]][[1]] <- coord_sets[[i]][[j]][[1]] - lowest_col + extra_left_space
      highest_col <- max(highest_col, coord_sets[[i]][[j]][[1]])
    }
  }
  
  matrix <- matrix(".", nrow = highest_row + 3, ncol = highest_col + extra_left_space * 2)
  
  for (set in coord_sets) {
    for (i in 2:length(set)) {
      cols <- sort(c(set[[i-1]][[1]], set[[i]][[1]]))
      rows <- sort(c(set[[i-1]][[2]], set[[i]][[2]]))
      
      if (cols[[1]] == cols[[2]]) {
        for (r in rows[[1]]:rows[[2]]) {
          matrix[r + 1, cols[[1]] + 1] <- "#"
        }
      } else {
        for (c in cols[[1]]:cols[[2]]) {
          matrix[rows[[1]] + 1, c + 1] <- "#"
        }
      }
    }
  }
  
  origin_col <- 500 - lowest_col + extra_left_space
  matrix[1, origin_col + 1] <- "+"
    for(i in 1:ncol(matrix)){
        matrix[nrow(matrix),i] <- "#"
    }
  
  return(list(matrix = matrix, origin_col = origin_col))
}

drop_sand <- function(matrix, origin_col) {
  r <- 1
  c <- origin_col + 1
  
  while (r < nrow(matrix)) {
    if (matrix[r + 1, c] == ".") {
      r <- r + 1
    } else if (matrix[r + 1, c - 1] == ".") {
      r <- r + 1
      c <- c - 1
    } else if (matrix[r + 1, c + 1] == ".") {
      r <- r + 1
      c <- c + 1
    } else {
      matrix[r, c] <- "o"
      return(list(matrix = matrix, fell_out = FALSE))
    }
  }
  
  return(list(matrix = matrix, fell_out = TRUE))
}

solve <- function(input) {
    parsed <- parse_input(input)
    matrix <- parsed$matrix
    origin_col <- parsed$origin_col

    ans <- 0
    repeat {
      result <- drop_sand(matrix, origin_col)
      matrix <- result$matrix
        if(result$fell_out) break

      ans <- ans + 1

      if (matrix[1, origin_col + 1] == "o") {
        break
      }

    }
    return(ans)
}
main <- function() {
  input_data <- readLines("input.txt")
  input_str <- paste(input_data, collapse = "\n")
  result <- solve(input_str)
  cat(result)
}

main()
