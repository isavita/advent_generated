
solve <- function() {
    grid <- readLines("input.txt")
    h <- length(grid)
    w <- nchar(grid[1])
    
    S <- NULL
    E <- NULL
    
    for (r in 1:h) {
      for (c in 1:w) {
        if (substr(grid[r], c, c) == 'S') {
          S <- c(r, c)
        } else if (substr(grid[r], c, c) == 'E') {
          E <- c(r, c)
        }
      }
    }
    
    track_cells <- matrix(ncol = 2, nrow = 0)
    walls <- matrix(FALSE, nrow = h, ncol = w)
    
    for (r in 1:h) {
      for (c in 1:w) {
        if (substr(grid[r], c, c) == '#') {
          walls[r, c] <- TRUE
        } else {
          track_cells <- rbind(track_cells, c(r, c))
        }
      }
    }

    dirs <- matrix(c(1, 0, -1, 0, 0, 1, 0, -1), ncol = 2, byrow = TRUE)

    bfs <- function(start) {
        dist <- matrix(-1, nrow = h, ncol = w)
        dist[start[1], start[2]] <- 0
        q <- matrix(start, ncol = 2)
        head <- 1
        tail <- 1

        while (head <= tail) {
            r <- q[head, 1]
            c <- q[head, 2]
            head <- head + 1

            for (i in 1:4) {
              dr <- dirs[i, 1]
              dc <- dirs[i, 2]
              nr <- r + dr
              nc <- c + dc

              if (nr >= 1 && nr <= h && nc >= 1 && nc <= w) {
                if (!walls[nr, nc] && dist[nr, nc] == -1) {
                  dist[nr, nc] <- dist[r, c] + 1
                  tail <- tail + 1
                  q <- rbind(q,c(nr,nc))
                }
              }
            }
        }
      return(dist)
    }

    dist_from_s <- bfs(S)
    dist_from_e <- bfs(E)

    if (dist_from_s[E[1], E[2]] == -1) {
        cat(0, "\n")
        return()
    }

    normal_cost <- dist_from_s[E[1], E[2]]

    possible_cheats <- 0
    
    for (i in 1:nrow(track_cells))
    {
      start_pos <- track_cells[i,]
      sd <- dist_from_s[start_pos[1], start_pos[2]]
      
      if(sd == -1)
        next
        
      for(j in 1:4)
      {
        m1r <- start_pos[1] + dirs[j,1]
        m1c <- start_pos[2] + dirs[j,2]
        
        if(!(m1r >= 1 && m1r <= h && m1c >= 1 && m1c <= w))
          next;
        
        for(k in 1:4)
        {
          m2r <- m1r + dirs[k,1]
          m2c <- m1c + dirs[k,2]

          if(!(m2r >= 1 && m2r <= h && m2c >= 1 && m2c <= w))
              next
          
          if(walls[m2r,m2c])
            next

          ed <- dist_from_e[m2r,m2c]

          if(ed == -1)
            next

          new_cost <- sd + 2 + ed
          if(normal_cost - new_cost >= 100)
            possible_cheats <- possible_cheats + 1
        }
      }
    }

    cat(possible_cheats, "\n")
}

solve()
