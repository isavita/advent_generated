options(expressions = 10000)

lines <- readLines("input.txt", warn = FALSE)
grid <- do.call(rbind, strsplit(lines, ""))
rows <- nrow(grid)
cols <- ncol(grid)

dr <- c(-1, 1, 0, 0)
dc <- c(0, 0, -1, 1)

start_node <- c(1, 2)
end_node <- c(rows, cols - 1)
junctions <- list(start_node, end_node)

junc_map <- matrix(0, rows, cols)
junc_map[1, 2] <- 1
junc_map[rows, cols - 1] <- 2

for (r in 1:rows) {
  for (c in 1:cols) {
    if (grid[r, c] == "." && junc_map[r, c] == 0) {
      neighbors <- 0
      for (k in 1:4) {
        nr <- r + dr[k]
        nc <- c + dc[k]
        if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols && grid[nr, nc] != "#") {
          neighbors <- neighbors + 1
        }
      }
      if (neighbors > 2) {
        junctions[[length(junctions) + 1]] <- c(r, c)
        junc_map[r, c] <- length(junctions)
      }
    }
  }
}

adj <- vector("list", length(junctions))
for (i in seq_along(junctions)) {
  adj[[i]] <- list()
  q <- list(list(p = junctions[[i]], d = 0))
  visited <- matrix(FALSE, rows, cols)
  visited[junctions[[i]][1], junctions[[i]][2]] <- TRUE
  
  head <- 1
  while (head <= length(q)) {
    curr <- q[[head]]
    head <- head + 1
    
    if (curr$d > 0 && junc_map[curr$p[1], curr$p[2]] > 0) {
      adj[[i]][[length(adj[[i]]) + 1]] <- c(junc_map[curr$p[1], curr$p[2]], curr$d)
      next
    }
    
    for (k in 1:4) {
      nr <- curr$p[1] + dr[k]
      nc <- curr$p[2] + dc[k]
      if (nr < 1 || nr > rows || nc < 1 || nc > cols || grid[nr, nc] == "#" || visited[nr, nc]) next
      
      char <- grid[nr, nc]
      ok <- TRUE
      if (char == "^" && dr[k] != -1) ok <- FALSE
      else if (char == "v" && dr[k] != 1) ok <- FALSE
      else if (char == "<" && dc[k] != -1) ok <- FALSE
      else if (char == ">" && dc[k] != 1) ok <- FALSE
      
      if (ok) {
        visited[nr, nc] <- TRUE
        q[[length(q) + 1]] <- list(p = c(nr, nc), d = curr$d + 1)
      }
    }
  }
}

max_dist <- -1
seen <- rep(FALSE, length(junctions))

dfs <- function(u, d) {
  if (u == 2) {
    if (d > max_dist) max_dist <<- d
    return()
  }
  seen[u] <<- TRUE
  for (edge in adj[[u]]) {
    if (!seen[edge[1]]) {
      dfs(edge[1], d + edge[2])
    }
  }
  seen[u] <<- FALSE
}

dfs(1, 0)
cat(max_dist, "\n")