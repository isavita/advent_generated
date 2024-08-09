read_input <- function(file) {
  grid <- readLines(file, warn = FALSE)
  strsplit(grid, "") 
}

transform <- function(grid) {
  new_grid <- grid
  for (i in seq_along(grid)) {
    for (j in seq_along(grid[[i]])) {
      new_grid[[i]][[j]] <- next_acre_state(grid, i, j)
    }
  }
  new_grid
}

next_acre_state <- function(grid, i, j) {
  acre <- grid[[i]][[j]]
  adj_count <- function(type) sum(sapply(-1:1, function(x) sapply(-1:1, function(y) {
    if (x == 0 && y == 0) return(0)
    ni <- i + x
    nj <- j + y
    if (ni >= 1 && ni <= length(grid) && nj >= 1 && nj <= length(grid[[ni]])) {
      return(grid[[ni]][[nj]] == type)
    }
    return(0)
  })))
  
  if (acre == '.') {
    if (adj_count('|') >= 3) return('|')
  } else if (acre == '|') {
    if (adj_count('#') >= 3) return('#')
  } else if (acre == '#') {
    if (adj_count('#') >= 1 && adj_count('|') >= 1) return('#')
    return('.')
  }
  acre
}

count_resources <- function(grid) {
  wooded <- sum(sapply(grid, function(row) sum(unlist(row) == '|')))
  lumberyards <- sum(sapply(grid, function(row) sum(unlist(row) == '#')))
  c(wooded, lumberyards)
}

grid_to_string <- function(grid) {
  paste(sapply(grid, paste, collapse = ""), collapse = "\n")
}

input_file <- "input.txt"
grid <- read_input(input_file)

seen_states <- list()
cycle_start <- cycle_length <- 0
minute <- 0

while (minute < 1000000000) {
  state <- grid_to_string(grid)
  if (state %in% names(seen_states)) {
    cycle_start <- seen_states[[state]]
    cycle_length <- minute - cycle_start
    break
  }
  seen_states[[state]] <- minute
  grid <- transform(grid)
  minute <- minute + 1
}

if (cycle_length > 0) {
  remaining_minutes <- (1000000000 - cycle_start) %% cycle_length
  for (i in seq_len(remaining_minutes)) {
    grid <- transform(grid)
  }
}

resources <- count_resources(grid)
print(resources[1] * resources[2])