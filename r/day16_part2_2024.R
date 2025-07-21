
main <- function() {
  lines <- readLines("input.txt")
  n <- length(lines)
  m <- nchar(lines[1])
  grid <- matrix(strsplit(paste(lines, collapse = ""), "")[[1]], nrow = n, byrow = TRUE)

  pos <- which(grid == "S", arr.ind = TRUE)
  sx <- pos[1, 1]; sy <- pos[1, 2]
  pos <- which(grid == "E", arr.ind = TRUE)
  ex <- pos[1, 1]; ey <- pos[1, 2]

  dx <- c(-1, 0, 1, 0)
  dy <- c(0, 1, 0, -1)

  dist <- array(Inf, dim = c(n, m, 4))
  dist[sx, sy, 2] <- 0
  pq <- list(c(0, sx, sy, 2))

  while (length(pq) > 0) {
    idx <- which.min(sapply(pq, `[[`, 1))
    cur <- pq[[idx]]
    pq[[idx]] <- NULL
    cost <- cur[1]; x <- cur[2]; y <- cur[3]; d <- cur[4]
    if (cost > dist[x, y, d]) next

    for (ndir in c((d %% 4) + 1, ((d + 2) %% 4) + 1)) {
      nc <- cost + 1000
      if (nc < dist[x, y, ndir]) {
        dist[x, y, ndir] <- nc
        pq[[length(pq) + 1]] <- c(nc, x, y, ndir)
      }
    }

    nx <- x + dx[d]; ny <- y + dy[d]
    if (nx >= 1 && nx <= n && ny >= 1 && ny <= m && grid[nx, ny] != "#") {
      nc <- cost + 1
      if (nc < dist[nx, ny, d]) {
        dist[nx, ny, d] <- nc
        pq[[length(pq) + 1]] <- c(nc, nx, ny, d)
      }
    }
  }

  best <- min(dist[ex, ey, ])
  rev <- list()
  vis <- array(FALSE, dim = c(n, m, 4))
  for (d in 1:4) {
    if (dist[ex, ey, d] == best) {
      rev[[length(rev) + 1]] <- c(ex, ey, d)
      vis[ex, ey, d] <- TRUE
    }
  }

  used <- matrix(FALSE, nrow = n, ncol = m)
  while (length(rev) > 0) {
    cur <- rev[[length(rev)]]
    rev <- rev[-length(rev)]
    x <- cur[1]; y <- cur[2]; d <- cur[3]
    used[x, y] <- TRUE
    costU <- dist[x, y, d]

    for (pd in c((d %% 4) + 1, ((d + 2) %% 4) + 1)) {
      if (dist[x, y, pd] == costU - 1000 && !vis[x, y, pd]) {
        vis[x, y, pd] <- TRUE
        rev[[length(rev) + 1]] <- c(x, y, pd)
      }
    }

    px <- x - dx[d]; py <- y - dy[d]
    if (px >= 1 && px <= n && py >= 1 && py <= m && grid[px, py] != "#" &&
        dist[px, py, d] == costU - 1 && !vis[px, py, d]) {
      vis[px, py, d] <- TRUE
      rev[[length(rev) + 1]] <- c(px, py, d)
    }
  }

  cat(sum(used))
}

main()
