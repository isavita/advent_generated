move_sea_cucumbers <- function(map) {
  rows <- nrow(map)
  cols <- ncol(map)
  moved <- TRUE
  steps <- 0
  
  while (moved) {
    moved <- FALSE
    new_map <- map
    
    # East-facing cucumbers move first
    for (i in 1:rows) {
      for (j in 1:cols) {
        if (map[i, j] == '>') {
          next_pos <- ifelse(j == cols, 1, j + 1)
          if (map[i, next_pos] == '.') {
            new_map[i, j] <- '.'
            new_map[i, next_pos] <- '>'
            moved <- TRUE
          }
        }
      }
    }
    
    map <- new_map
    
    # South-facing cucumbers move next
    new_map <- map
    for (j in 1:cols) {
      for (i in 1:rows) {
        if (map[i, j] == 'v') {
          next_pos <- ifelse(i == rows, 1, i + 1)
          if (map[next_pos, j] == '.') {
            new_map[i, j] <- '.'
            new_map[next_pos, j] <- 'v'
            moved <- TRUE
          }
        }
      }
    }
    
    map <- new_map
    steps <- steps + 1
  }
  
  return(steps)
}

main <- function() {
  input <- readLines("input.txt")
  map <- do.call(rbind, strsplit(input, ""))
  result <- move_sea_cucumbers(map)
  cat(result, "\n")
}

main()