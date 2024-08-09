read_input <- function(file) {
  lines <- readLines(file)
  discs <- lapply(lines, function(line) {
    matches <- regmatches(line, regexec("Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+).", line))
    list(total_positions = as.integer(matches[[1]][3]), start_position = as.integer(matches[[1]][4]))
  })
  discs[[length(discs) + 1]] <- list(total_positions = 11, start_position = 0)
  return(discs)
}

check_discs <- function(discs, time) {
  all(sapply(seq_along(discs), function(i) {
    position <- (discs[[i]]$start_position + time + i) %% discs[[i]]$total_positions
    position == 0
  }))
}

discs <- read_input("input.txt")
time <- 0
while (!check_discs(discs, time)) {
  time <- time + 1
}
print(time)