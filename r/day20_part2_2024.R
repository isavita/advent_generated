
main <- function() {
  lines <- readLines("input.txt")
  H <- length(lines)
  W <- nchar(lines[1])
  grid <- matrix(strsplit(paste(lines, collapse = ""), "")[[1]], nrow = H, byrow = TRUE)
  
  S <- which(grid == "S", arr.ind = TRUE)[1, ]
  E <- which(grid == "E", arr.ind = TRUE)[1, ]
  walls <- grid == "#"
  
  bfs <- function(start) {
    dist <- array(-1, dim = dim(grid))
    q <- list()
    dist[start[1], start[2]] <- 0
    q <- c(q, list(start))
    while (length(q) > 0) {
      curr <- q[[1]]
      q <- q[-1]
      d <- dist[curr[1], curr[2]]
      for (dir in list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))) {
        nr <- curr[1] + dir[1]
        nc <- curr[2] + dir[2]
        if (nr >= 1 && nr <= H && nc >= 1 && nc <= W && !walls[nr, nc] && dist[nr, nc] == -1) {
          dist[nr, nc] <- d + 1
          q <- c(q, list(c(nr, nc)))
        }
      }
    }
    dist
  }
  
  dist_s <- bfs(S)
  dist_e <- bfs(E)
  normal_cost <- dist_s[E[1], E[2]]
  
  if (normal_cost == -1) {
    cat("0\n")
    return()
  }
  
  cheat_count <- 0
  
  for (sr in 1:H) {
    for (sc in 1:W) {
      if (walls[sr, sc] || dist_s[sr, sc] == -1) next
      dist_c <- array(-1, dim = dim(grid))
      q <- list()
      dist_c[sr, sc] <- 0
      q <- c(q, list(c(sr, sc)))
      while (length(q) > 0) {
        curr <- q[[1]]
        q <- q[-1]
        d <- dist_c[curr[1], curr[2]]
        if (d >= 20) next
        for (dir in list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))) {
          nr <- curr[1] + dir[1]
          nc <- curr[2] + dir[2]
          if (nr >= 1 && nr <= H && nc >= 1 && nc <= W && dist_c[nr, nc] == -1) {
            dist_c[nr, nc] <- d + 1
            q <- c(q, list(c(nr, nc)))
          }
        }
      }
      
      for (er in 1:H) {
        for (ec in 1:W) {
          if (walls[er, ec] || dist_e[er, ec] == -1) next
          s <- dist_c[er, ec]
          if (s > 0 && s <= 20) {
            total_cost <- dist_s[sr, sc] + s + dist_e[er, ec]
            if (total_cost < normal_cost && (normal_cost - total_cost) >= 100) {
              cheat_count <- cheat_count + 1
            }
          }
        }
      }
    }
  }
  
  cat(cheat_count, "\n")
}

main()
