
lines <- readLines("input.txt")
pairs_list <- lapply(strsplit(lines, "[: ]+"), function(x) {
  if (length(x) < 2) return(NULL)
  cbind(x[1], x[-1])
})
pairs <- do.call(rbind, pairs_list)
nodes <- unique(as.vector(pairs))
n_nodes <- length(nodes)
node_map <- setNames(seq_along(nodes), nodes)
u_ids <- as.integer(node_map[pairs[, 1]])
v_ids <- as.integer(node_map[pairs[, 2]])
n_edges <- length(u_ids)

tmp_u <- c(u_ids, v_ids)
tmp_e <- c(seq_len(n_edges), seq_len(n_edges))
adj <- split(tmp_e, factor(tmp_u, levels = seq_len(n_nodes)))
names(adj) <- NULL

bfs <- function(src, sink, active_edges) {
  parent_edge <- integer(n_nodes)
  visited <- logical(n_nodes)
  visited[src] <- TRUE
  q <- integer(n_nodes)
  q[1] <- src
  head <- 1
  tail <- 2
  found <- FALSE
  while (head < tail) {
    u <- q[head]
    head <- head + 1
    if (u == sink) {
      found <- TRUE
      break
    }
    for (e_idx in adj[[u]]) {
      if (active_edges[e_idx]) {
        v <- if (u_ids[e_idx] == u) v_ids[e_idx] else u_ids[e_idx]
        if (!visited[v]) {
          visited[v] <- TRUE
          parent_edge[v] <- e_idx
          q[tail] <- v
          tail <- tail + 1
        }
      }
    }
  }
  list(found = found, visited = visited, parent_edge = parent_edge)
}

for (sink in 2:n_nodes) {
  active_edges <- rep(TRUE, n_edges)
  flow <- 0
  for (k in 1:3) {
    res <- bfs(1, sink, active_edges)
    if (!res$found) break
    flow <- flow + 1
    curr <- sink
    while (curr != 1) {
      e_idx <- res$parent_edge[curr]
      active_edges[e_idx] <- FALSE
      curr <- if (u_ids[e_idx] == curr) v_ids[e_idx] else u_ids[e_idx]
    }
  }
  if (flow == 3) {
    res <- bfs(1, sink, active_edges)
    if (!res$found) {
      size1 <- sum(res$visited)
      cat(size1 * (n_nodes - size1), "\n")
      break
    }
  }
}
