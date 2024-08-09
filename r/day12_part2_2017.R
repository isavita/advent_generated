read_input <- function(file) {
  connections <- list()
  con <- file(file, "r")
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    parts <- strsplit(line, " <-> ")[[1]]
    id <- as.integer(parts[1])
    neighbors <- as.integer(unlist(strsplit(parts[2], ", ")))
    connections[[as.character(id)]] <- neighbors
  }
  close(con)
  return(connections)
}

dfs <- function(graph, start, visited) {
  stack <- list(start)
  while (length(stack) > 0) {
    node <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    if (!node %in% visited) {
      visited <- c(visited, node)
      stack <- c(stack, graph[[as.character(node)]])
    }
  }
  return(visited)
}

count_group_size <- function(graph, id) {
  visited <- dfs(graph, id, character())
  return(length(visited))
}

count_groups <- function(graph) {
  visited <- character()
  groups <- 0
  for (node in names(graph)) {
    if (!(node %in% visited)) {
      visited <- dfs(graph, as.integer(node), visited)
      groups <- groups + 1
    }
  }
  return(groups)
}

main <- function() {
  connections <- read_input("input.txt")
  group_size <- count_group_size(connections, 0)
  total_groups <- count_groups(connections)
  
  cat("Programs in the group containing ID 0:", group_size, "\n")
  cat("Total number of groups:", total_groups, "\n")
}

main()