# Read input from the file
lines <- readLines("input.txt")

# Create an adjacency list for the graph
graph <- list()

for (line in lines) {
  parts <- strsplit(line, " <-> ")[[1]]
  id <- as.integer(parts[1])
  connections <- as.integer(unlist(strsplit(parts[2], ", ")))
  graph[[as.character(id)]] <- connections
}

# Function to perform DFS and count connected programs
count_connected <- function(start_id, graph) {
  visited <- integer(0)
  stack <- c(start_id)
  
  while (length(stack) > 0) {
    current <- stack[length(stack)]
    stack <- stack[-length(stack)]
    
    if (!(current %in% visited)) {
      visited <- c(visited, current)
      stack <- c(stack, graph[[as.character(current)]])
    }
  }
  
  return(length(visited))
}

# Count programs in the group containing program ID 0
result <- count_connected(0, graph)
cat(result, "\n")