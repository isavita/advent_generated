# Function to determine if a coordinate is a wall or open space
is_open_space <- function(x, y, favorite_number) {
  if (x < 0 || y < 0) return(FALSE)
  value <- x^2 + 3*x + 2*x*y + y + y^2 + favorite_number
  return(sum(as.numeric(intToBits(value))) %% 2 == 0)
}

# BFS to find the shortest path
bfs_shortest_path <- function(start, goal, favorite_number) {
  queue <- list(start)
  visited <- list(start)
  steps <- 0
  
  directions <- list(c(1, 0), c(0, 1), c(-1, 0), c(0, -1))
  
  while (length(queue) > 0) {
    next_queue <- list()
    for (pos in queue) {
      if (identical(pos, goal)) return(steps)
      for (dir in directions) {
        neighbor <- c(pos[1] + dir[1], pos[2] + dir[2])
        if (is_open_space(neighbor[1], neighbor[2], favorite_number) && 
            !any(sapply(visited, identical, neighbor))) {
          visited <- append(visited, list(neighbor))
          next_queue <- append(next_queue, list(neighbor))
        }
      }
    }
    queue <- next_queue
    steps <- steps + 1
  }
  return(-1) # If goal is not reachable
}

# Main function to read input and execute the search
main <- function() {
  favorite_number <- as.integer(readLines("input.txt"))
  start <- c(1, 1)
  goal <- c(31, 39)
  steps <- bfs_shortest_path(start, goal, favorite_number)
  cat(steps, "\n")
}

main()