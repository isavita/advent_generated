options(scipen = 999)

grid_str <- readLines("input.txt")
H <- length(grid_str)
W <- nchar(grid_str[1])
grid <- matrix(as.integer(unlist(strsplit(grid_str, ""))), nrow = H, ncol = W, byrow = TRUE)

solve <- function(min_s, max_s) {
  dr <- c(-1, 1, 0, 0)
  dc <- c(0, 0, 1, -1)
  rev_d <- c(2, 1, 4, 3)
  dist <- array(Inf, dim = c(H, W, 4, max_s))
  
  h_cost <- numeric(1000000)
  h_r <- integer(1000000)
  h_c <- integer(1000000)
  h_d <- integer(1000000)
  h_s <- integer(1000000)
  h_n <- 0
  
  push <- function(co, r, c, d, s) {
    h_n <<- h_n + 1
    h_cost[h_n] <<- co; h_r[h_n] <<- r; h_c[h_n] <<- c; h_d[h_n] <<- d; h_s[h_n] <<- s
    i <- h_n
    while(i > 1) {
      p <- i %/% 2
      if(h_cost[p] <= h_cost[i]) break
      t_co <- h_cost[i]; h_cost[i] <<- h_cost[p]; h_cost[p] <<- t_co
      t_r <- h_r[i]; h_r[i] <<- h_r[p]; h_r[p] <<- t_r
      t_c <- h_c[i]; h_c[i] <<- h_c[p]; h_c[p] <<- t_c
      t_d <- h_d[i]; h_d[i] <<- h_d[p]; h_d[p] <<- t_d
      t_s <- h_s[i]; h_s[i] <<- h_s[p]; h_s[p] <<- t_s
      i <- p
    }
  }
  
  pop <- function() {
    res <- c(h_cost[1], h_r[1], h_c[1], h_d[1], h_s[1])
    h_cost[1] <<- h_cost[h_n]; h_r[1] <<- h_r[h_n]; h_c[1] <<- h_c[h_n]; h_d[1] <<- h_d[h_n]; h_s[1] <<- h_s[h_n]
    h_n <<- h_n - 1
    i <- 1
    while(TRUE) {
      l <- i + i; r_idx <- l + 1; s_idx <- i
      if(l <= h_n && h_cost[l] < h_cost[s_idx]) s_idx <- l
      if(r_idx <= h_n && h_cost[r_idx] < h_cost[s_idx]) s_idx <- r_idx
      if(s_idx == i) break
      t_co <- h_cost[i]; h_cost[i] <<- h_cost[s_idx]; h_cost[s_idx] <<- t_co
      t_r <- h_r[i]; h_r[i] <<- h_r[s_idx]; h_r[s_idx] <<- t_r
      t_c <- h_c[i]; h_c[i] <<- h_c[s_idx]; h_c[s_idx] <<- t_c
      t_d <- h_d[i]; h_d[i] <<- h_d[s_idx]; h_d[s_idx] <<- t_d
      t_s <- h_s[i]; h_s[i] <<- h_s[s_idx]; h_s[s_idx] <<- t_s
      i <- s_idx
    }
    res
  }
  
  if(W > 1) { dist[1, 2, 3, 1] <- grid[1, 2]; push(grid[1, 2], 1, 2, 3, 1) }
  if(H > 1) { dist[2, 1, 2, 1] <- grid[2, 1]; push(grid[2, 1], 2, 1, 2, 1) }
  
  while(h_n > 0) {
    cur <- pop()
    co <- cur[1]; r <- cur[2]; c <- cur[3]; d <- cur[4]; s <- cur[5]
    if(co > dist[r, c, d, s]) next
    if(r == H && c == W && s >= min_s) return(co)
    
    for(nd in 1:4) {
      if(nd == rev_d[d]) next
      nr <- r + dr[nd]; nc <- c + dc[nd]
      if(nr >= 1 && nr <= H && nc >= 1 && nc <= W) {
        ns <- if(nd == d) s + 1 else 1
        if(!((nd == d && ns > max_s) || (nd != d && s < min_s))) {
          ncost <- co + grid[nr, nc]
          if(ncost < dist[nr, nc, nd, ns]) {
            dist[nr, nc, nd, ns] <- ncost
            push(ncost, nr, nc, nd, ns)
          }
        }
      }
    }
  }
}

cat(as.integer(solve(1, 3)), "\n")
cat(as.integer(solve(4, 10)), "\n")