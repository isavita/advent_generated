
connections <- readLines("input.txt")
edges <- strsplit(connections, "-")
graph <- list()
for (edge in edges) {
  if (length(edge) == 2) {
    u <- edge[1]
    v <- edge[2]
    if (is.null(graph[[u]])) {
      graph[[u]] <- c()
    }
    if (is.null(graph[[v]])) {
      graph[[v]] <- c()
    }
    graph[[u]] <- c(graph[[u]], v)
    graph[[v]] <- c(graph[[v]], u)
  }
}
computers <- names(graph)
n <- length(computers)
triplets <- 0
for (i in 1:(n - 2)) {
  for (j in (i + 1):(n - 1)) {
    for (k in (j + 1):n) {
      c1 <- computers[i]
      c2 <- computers[j]
      c3 <- computers[k]
      if (c2 %in% graph[[c1]] && c3 %in% graph[[c2]] && c3 %in% graph[[c1]]) {
        if (startsWith(c1, "t") || startsWith(c2, "t") || startsWith(c3, "t")) {
          triplets <- triplets + 1
        }
      }
    }
  }
}
cat("Number of triplets containing at least one computer with name starting with 't':", triplets, "\n")
