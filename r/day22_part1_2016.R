read_nodes <- function(filename) {
  con <- file(filename, "r")
  on.exit(close(con))
  nodes <- list()
  pattern <- "node-x\\d+-y\\d+\\s+\\d+T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%"
  
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    matches <- regmatches(line, regexec(pattern, line))
    if (length(matches[[1]]) > 2) {
      used <- as.integer(matches[[1]][2])
      avail <- as.integer(matches[[1]][3])
      nodes <- append(nodes, list(c(used, avail)))
    }
  }
  return(nodes)
}

count_viable_pairs <- function(nodes) {
  count <- 0
  for (i in seq_along(nodes)) {
    for (j in seq_along(nodes)) {
      if (i != j && nodes[[i]][1] > 0 && nodes[[i]][1] <= nodes[[j]][2]) {
        count <- count + 1
      }
    }
  }
  return(count)
}

nodes <- read_nodes("input.txt")
viable_pairs <- count_viable_pairs(nodes)
print(viable_pairs)