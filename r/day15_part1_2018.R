
solve <- function() {
  lines <- readLines("input.txt")
  nr <- length(lines)
  nc <- nchar(lines[1])
  grid <- do.call(rbind, strsplit(lines, ""))
  
  units <- data.frame(y = integer(), x = integer(), type = character(), hp = integer(), stringsAsFactors = FALSE)
  for (r in 1:nr) {
    for (c in 1:nc) {
      if (grid[r, c] %in% c("E", "G")) {
        units <- rbind(units, data.frame(y = r, x = c, type = grid[r, c], hp = 200))
        grid[r, c] <- "."
      }
    }
  }
  
  adj_offsets <- list(c(-1, 0), c(0, -1), c(0, 1), c(1, 0))
  
  bfs <- function(start_y, start_x, target_map) {
    dist <- matrix(NA, nr, nc)
    dist[start_y, start_x] <- 0
    q <- list(c(start_y, start_x))
    head <- 1
    
    while(head <= length(q)) {
      curr <- q[[head]]; head <- head + 1
      d <- dist[curr[1], curr[2]]
      for (off in adj_offsets) {
        ny <- curr[1] + off[1]; nx <- curr[2] + off[2]
        if (grid[ny, nx] == "." && is.na(dist[ny, nx])) {
          # Check if occupied by another unit
          occupied <- FALSE
          for (i in 1:nrow(units)) {
            if (units$hp[i] > 0 && units$y[i] == ny && units$x[i] == nx) {
              occupied <- TRUE; break
            }
          }
          if (!occupied) {
            dist[ny, nx] <- d + 1
            q[[length(q) + 1]] <- c(ny, nx)
          }
        }
      }
    }
    dist
  }

  rounds <- 0
  repeat {
    units <- units[order(units$y, units$x), ]
    interrupted <- FALSE
    
    for (i in 1:nrow(units)) {
      if (units$hp[i] <= 0) next
      
      enemies <- which(units$type != units$type[i] & units$hp > 0)
      if (length(enemies) == 0) {
        interrupted <- TRUE; break
      }
      
      # Try move
      is_adj <- FALSE
      for (off in adj_offsets) {
        ny <- units$y[i] + off[1]; nx <- units$x[i] + off[2]
        for (e_idx in enemies) {
          if (units$y[e_idx] == ny && units$x[e_idx] == nx) {
            is_adj <- TRUE; break
          }
        }
        if (is_adj) break
      }
      
      if (!is_adj) {
        dists <- bfs(units$y[i], units$x[i])
        in_range <- list()
        for (e_idx in enemies) {
          for (off in adj_offsets) {
            ny <- units$y[e_idx] + off[1]; nx <- units$x[e_idx] + off[2]
            if (grid[ny, nx] == "." && !is.na(dists[ny, nx])) {
              occ <- FALSE
              for (u_j in 1:nrow(units)) {
                if (units$hp[u_j] > 0 && units$y[u_j] == ny && units$x[u_j] == nx) { occ <- TRUE; break }
              }
              if (!occ) in_range[[length(in_range) + 1]] <- c(ny, nx, dists[ny, nx])
            }
          }
        }
        
        if (length(in_range) > 0) {
          in_range <- do.call(rbind, in_range)
          min_dist <- min(in_range[, 3])
          candidates <- in_range[in_range[, 3] == min_dist, , drop = FALSE]
          candidates <- candidates[order(candidates[, 1], candidates[, 2]), , drop = FALSE]
          target_tile <- candidates[1, 1:2]
          
          # Move step
          best_step <- NULL
          min_step_dist <- Inf
          for (off in adj_offsets) {
            ny <- units$y[i] + off[1]; nx <- units$x[i] + off[2]
            if (grid[ny, nx] == ".") {
              occ <- FALSE
              for (u_j in 1:nrow(units)) {
                if (units$hp[u_j] > 0 && units$y[u_j] == ny && units$x[u_j] == nx) { occ <- TRUE; break }
              }
              if (!occ) {
                d2 <- bfs(ny, nx)
                d_to_target <- d2[target_tile[1], target_tile[2]]
                if (!is.na(d_to_target) && d_to_target < min_step_dist) {
                  min_step_dist <- d_to_target
                  best_step <- c(ny, nx)
                }
              }
            }
          }
          if (!is.null(best_step)) {
            units$y[i] <- best_step[1]
            units$x[i] <- best_step[2]
          }
        }
      }
      
      # Attack
      target_idx <- -1
      min_hp <- 201
      for (off in adj_offsets) {
        ny <- units$y[i] + off[1]; nx <- units$x[i] + off[2]
        for (e_idx in enemies) {
          if (units$y[e_idx] == ny && units$x[e_idx] == nx) {
            if (units$hp[e_idx] < min_hp) {
              min_hp <- units$hp[e_idx]
              target_idx <- e_idx
            }
          }
        }
      }
      if (target_idx != -1) {
        units$hp[target_idx] <- units$hp[target_idx] - 3
      }
    }
    
    if (interrupted) break
    rounds <- rounds + 1
    units <- units[units$hp > 0, ]
  }
  
  cat(rounds * sum(units$hp[units$hp > 0]), "\n")
}

solve()
