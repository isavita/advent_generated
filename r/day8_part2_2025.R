
lines <- readLines("input.txt", warn = FALSE)
lines <- lines[nchar(trimws(lines)) > 0]
pts <- do.call(rbind, lapply(strsplit(lines, ","), as.integer))
n <- nrow(pts)

if (n >= 2) {
  edges <- t(combn(n, 2))
  dx <- pts[edges[,1], 1] - pts[edges[,2], 1]
  dy <- pts[edges[,1], 2] - pts[edges[,2], 2]
  dz <- pts[edges[,1], 3] - pts[edges[,2], 3]
  distsSq <- as.numeric(dx)^2 + as.numeric(dy)^2 + as.numeric(dz)^2
  
  order_idx <- order(distsSq)
  edges <- edges[order_idx, ]
  
  parent <- 1:n
  find <- function(i) {
    if (parent[i] == i) return(i)
    parent[i] <<- find(parent[i])
    return(parent[i])
  }
  
  comps <- n
  for (i in 1:nrow(edges)) {
    u <- edges[i, 1]
    v <- edges[i, 2]
    root_u <- find(u)
    root_v <- find(v)
    
    if (root_u != root_v) {
      parent[root_u] <- root_v
      comps <- comps - 1
      if (comps == 1) {
        p1 <- pts[u, ]
        p2 <- pts[v, ]
        cat(sprintf("Connected %d,%d,%d and %d,%d,%d\n", p1[1], p1[2], p1[3], p2[1], p2[2], p2[3]))
        cat(sprintf("Product of X coordinates: %.0f\n", as.numeric(p1[1]) * p2[1]))
        break
      }
    }
  }
}
