
solve <- function() {
  byte_positions <- read.csv("input.txt", header = FALSE)
  grid_size <- 71

  is_valid <- function(r, c, grid) {
    return(r >= 1 && r <= grid_size && c >= 1 && c <= grid_size && grid[r, c] == ".")
  }

  bfs <- function(grid) {
    q <- matrix(c(1, 1, 0), nrow = 1, byrow = TRUE)
    visited <- matrix(FALSE, nrow = grid_size, ncol = grid_size)
    visited[1, 1] <- TRUE

    while (nrow(q) > 0) {
      r <- q[1, 1]
      c <- q[1, 2]
      dist <- q[1, 3]
      q <- q[-1, , drop = FALSE]

      if (r == grid_size && c == grid_size) {
        return(dist)
      }

      for (dr in c(0, 0, 1, -1)) {
        for (dc in c(1, -1, 0, 0)) {
          if (dr != 0 && dc != 0) next
          nr <- r + dr
          nc <- c + dc
          if (is_valid(nr, nc, grid) && !visited[nr, nc]) {
            visited[nr, nc] <- TRUE
            q <- rbind(q, c(nr, nc, dist + 1))
          }
        }
      }
    }
    return(-1)
  }

  grid <- matrix(".", nrow = grid_size, ncol = grid_size)
  for (i in 1:min(1024, nrow(byte_positions))) {
    x <- byte_positions[i, 1] + 1
    y <- byte_positions[i, 2] + 1
    grid[y, x] <- "#"
  }

  part1_result <- bfs(grid)
  cat(part1_result, "\n")

  grid <- matrix(".", nrow = grid_size, ncol = grid_size)
  for (i in 1:nrow(byte_positions)) {
    x <- byte_positions[i, 1] + 1
    y <- byte_positions[i, 2] + 1
    grid[y, x] <- "#"
    if (bfs(grid) == -1) {
      cat(byte_positions[i,1], ",", byte_positions[i,2] , "\n", sep="")
      break
    }
  }
}

main <- function() {
    solve()
}

main()
