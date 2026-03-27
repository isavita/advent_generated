
options(expressions = 50000)

input <- readLines("input.txt", warn = FALSE)
if (length(input) == 0) quit()

grid <- do.call(rbind, strsplit(input, ""))
nr <- nrow(grid)
nc <- ncol(grid)

node_map <- matrix(0, nr, nc)
nodes <- list(c(1, 2), c(nr, nc - 1))
node_map[1, 2] <- 1
node_map[nr, nc - 1] <- 2

for (r in 1:nr) {
  for (c in 1:nc) {
    if (grid[r, c] != "#" && node_map[r, c] == 0) {
      nbs <- 0
      if (r > 1 && grid[r - 1, c] != "#") nbs <- nbs + 1
      if (r < nr && grid[r + 1, c] != "#") nbs <- nbs + 1
      if (c > 1 && grid[r, c - 1] != "#") nbs <- nbs + 1
      if (c < nc && grid[r, c + 1] != "#") nbs <- nbs + 1
      if (nbs > 2) {
        nodes[[length(nodes) + 1]] <- c(r, c)
        node_map[r, c] <- length(nodes)
      }
    }
  }
}

num_nodes <- length(nodes)
adj_matrix <- matrix(0, num_nodes, num_nodes)

for (i in 1:num_nodes) {
  start_node <- nodes[[i]]
  q_r <- integer(nr * nc)
  q_c <- integer(nr * nc)
  q_d <- integer(nr * nc)
  q_r[1] <- start_node[1]
  q_c[1] <- start_node[2]
  q_d[1] <- 0
  
  visited <- matrix(FALSE, nr, nc)
  visited[start_node[1], start_node[2]] <- TRUE
  
  head <- 1
  tail <- 2
  while (head < tail) {
    r <- q_r[head]
    c <- q_c[head]
    d <- q_d[head]
    head <- head + 1
    
    if (node_map[r, c] != 0 && node_map[r, c] != i) {
      adj_matrix[i, node_map[r, c]] <- d
      next
    }
    
    if (r > 1 && grid[r - 1, c] != "#" && !visited[r - 1, c]) {
      visited[r - 1, c] <- TRUE
      q_r[tail] <- r - 1; q_c[tail] <- c; q_d[tail] <- d + 1
      tail <- tail + 1
    }
    if (r < nr && grid[r + 1, c] != "#" && !visited[r + 1, c]) {
      visited[r + 1, c] <- TRUE
      q_r[tail] <- r + 1; q_c[tail] <- c; q_d[tail] <- d + 1
      tail <- tail + 1
    }
    if (c > 1 && grid[r, c - 1] != "#" && !visited[r, c - 1]) {
      visited[r, c - 1] <- TRUE
      q_r[tail] <- r; q_c[tail] <- c - 1; q_d[tail] <- d + 1
      tail <- tail + 1
    }
    if (c < nc && grid[r, c + 1] != "#" && !visited[r, c + 1]) {
      visited[r, c + 1] <- TRUE
      q_r[tail] <- r; q_c[tail] <- c + 1; q_d[tail] <- d + 1
      tail <- tail + 1
    }
  }
}

adj_to <- lapply(1:num_nodes, function(x) which(adj_matrix[x, ] > 0))
adj_w <- lapply(1:num_nodes, function(x) adj_matrix[x, adj_to[[x]]])

max_d <- 0
seen <- rep(FALSE, num_nodes)

solve_dfs <- function(u, dist) {
  if (u == 2) {
    if (dist > max_d) max_d <<- dist
    return()
  }
  seen[u] <<- TRUE
  neighbors <- adj_to[[u]]
  weights <- adj_w[[u]]
  if (length(neighbors) > 0) {
    for (j in 1:length(neighbors)) {
      v <- neighbors[j]
      if (!seen[v]) {
        solve_dfs(v, dist + weights[j])
      }
    }
  }
  seen[u] <<- FALSE
}

solve_dfs(1, 0)
cat(max_d, "\n")
