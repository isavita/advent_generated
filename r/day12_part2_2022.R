
main <- function() {
  grid <- list()
  start <- end <- NULL
  as_list <- list()
  
  lines <- readLines("input.txt")
  for (y in seq_along(lines)) {
    line <- lines[y]
    for (x in seq_along(strsplit(line, "")[[1]])) {
      b <- substr(line, x, x)
      p <- paste(x - 1, y - 1, sep = ",")
      grid[[p]] <- b
      if (b == "S") start <- p
      else if (b == "E") end <- p
      else if (b == "a") as_list <- c(as_list, p)
    }
  }
  grid[[start]] <- "a"
  grid[[end]] <- "z"
  
  dists <- dijkstra(grid, end)
  l <- dists[[start]]
  for (a in as_list) {
    d <- dists[[a]]
    if (!is.null(d)) l <- min(l, d)
  }
  cat(l, "\n")
}

dijkstra <- function(grid, end) {
  pq <- list(list(0, end))
  dist <- list()
  dist[[end]] <- 0
  
  while (length(pq) > 0) {
    curr <- pq[[1]]
    pq <- pq[-1]
    curr_dist <- curr[[1]]
    curr_pos <- curr[[2]]
    
    for (n in list(c(0, 1), c(0, -1), c(1, 0), c(-1, 0))) {
      next_x <- as.numeric(strsplit(curr_pos, ",")[[1]][1]) + n[1]
      next_y <- as.numeric(strsplit(curr_pos, ",")[[1]][2]) + n[2]
      next_p <- paste(next_x, next_y, sep = ",")
      
      if (is.null(grid[[next_p]])) next
      
      if (utf8ToInt(grid[[curr_pos]]) - utf8ToInt(grid[[next_p]]) > 1) next
      
      next_dist <- dist[[curr_pos]] + 1
      if (is.null(dist[[next_p]]) || next_dist < dist[[next_p]]) {
        dist[[next_p]] <- next_dist
        pq <- c(pq, list(list(next_dist, next_p)))
      }
    }
  }
  dist
}

main()
