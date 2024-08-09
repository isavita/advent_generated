simulate_octopuses <- function(grid, steps) {
  flashes <- 0
  directions <- expand.grid(-1:1, -1:1)[-5, ]  # All 8 possible directions excluding (0,0)

  for (step in 1:steps) {
    grid <- grid + 1
    flashed <- matrix(FALSE, nrow = 10, ncol = 10)
    
    repeat {
      new_flashes <- grid > 9 & !flashed
      if (!any(new_flashes)) break
      
      flashes <- flashes + sum(new_flashes)
      flashed <- flashed | new_flashes
      
      for (i in 1:nrow(directions)) {
        r_offset <- directions[i, 1]
        c_offset <- directions[i, 2]
        for (r in 1:10) {
          for (c in 1:10) {
            if (new_flashes[r, c]) {
              rr <- r + r_offset
              cc <- c + c_offset
              if (rr >= 1 && rr <= 10 && cc >= 1 && cc <= 10) {
                grid[rr, cc] <- grid[rr, cc] + 1
              }
            }
          }
        }
      }
    }
    
    grid[flashed] <- 0
  }
  
  return(flashes)
}

main <- function() {
  input <- readLines("input.txt")
  grid <- matrix(as.numeric(unlist(strsplit(input, ""))), nrow = 10, byrow = TRUE)
  total_flashes <- simulate_octopuses(grid, 100)
  cat(total_flashes, "\n")
}

main()