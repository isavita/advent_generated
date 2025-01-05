
# Function to determine if a coordinate is a wall or open space
is_wall <- function(x, y, favorite_number) {
  if (x < 0 || y < 0) return(TRUE) # Treat negative coordinates as walls
  val <- x*x + 3*x + 2*x*y + y + y*y + favorite_number
  sum(as.integer(intToBits(val))) %% 2 != 0
}

# Function to find reachable locations within a maximum number of steps
find_reachable <- function(start_x, start_y, favorite_number, max_steps) {
  visited <- matrix(FALSE, nrow = 100, ncol = 100) # Initialize a matrix to track visited locations
  queue <- list(c(start_x, start_y, 0)) # Queue of locations to explore, with steps
  visited[start_x + 1, start_y + 1] <- TRUE # Mark starting location as visited
  reachable_count <- 0
  
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    x <- current[1]
    y <- current[2]
    steps <- current[3]
    
    if (steps <= max_steps) {
      reachable_count <- reachable_count + 1
    }
    
    if (steps < max_steps) {
      # Explore neighbors (up, down, left, right)
      neighbors <- list(c(x, y - 1), c(x, y + 1), c(x - 1, y), c(x + 1, y))
      for (neighbor in neighbors) {
        nx <- neighbor[1]
        ny <- neighbor[2]
        if (nx >= 0 && ny >= 0 && !is_wall(nx, ny, favorite_number) && !visited[nx + 1, ny + 1]) {
          queue <- append(queue, list(c(nx, ny, steps + 1)))
          visited[nx + 1, ny + 1] <- TRUE
        }
      }
    }
  }
  return(reachable_count)
}

# Read input from file
input <- readLines("input.txt")
favorite_number <- as.integer(input[1])

# Part 1: Find shortest path to 31,39
start_x <- 1
start_y <- 1
target_x <- 31
target_y <- 39

# Use BFS to find shortest path
queue <- list(c(start_x, start_y, 0))
visited <- matrix(FALSE, nrow = 100, ncol = 100)
visited[start_x + 1, start_y + 1] <- TRUE
shortest_path <- Inf

while (length(queue) > 0) {
  current <- queue[[1]]
  queue <- queue[-1]
  x <- current[1]
  y <- current[2]
  steps <- current[3]
  
  if (x == target_x && y == target_y) {
    shortest_path <- steps
    break
  }
  
  neighbors <- list(c(x, y - 1), c(x, y + 1), c(x - 1, y), c(x + 1, y))
  for (neighbor in neighbors) {
    nx <- neighbor[1]
    ny <- neighbor[2]
    if (nx >= 0 && ny >= 0 && !is_wall(nx, ny, favorite_number) && !visited[nx + 1, ny + 1]) {
      queue <- append(queue, list(c(nx, ny, steps + 1)))
      visited[nx + 1, ny + 1] <- TRUE
    }
  }
}

cat("Part 1:", shortest_path, "\n")

# Part 2: Find reachable locations within 50 steps
max_steps <- 50
reachable_count <- find_reachable(start_x, start_y, favorite_number, max_steps)
cat("Part 2:", reachable_count, "\n")
