
data <- read.table("input.txt", header = FALSE)
head <- c(0, 0)
tail <- c(0, 0)
visited <- matrix(0, nrow = 1, ncol = 2)
visited[1,] <- tail

for (i in 1:nrow(data)) {
  dir <- as.character(data[i, 1])
  steps <- as.numeric(data[i, 2])
  
  for (j in 1:steps) {
    if (dir == "R") {
      head[1] <- head[1] + 1
    } else if (dir == "L") {
      head[1] <- head[1] - 1
    } else if (dir == "U") {
      head[2] <- head[2] + 1
    } else if (dir == "D") {
      head[2] <- head[2] - 1
    }
    
    if (abs(head[1] - tail[1]) > 1 || abs(head[2] - tail[2]) > 1) {
      if (head[1] != tail[1] && head[2] != tail[2]) {
        if (head[1] > tail[1]) {
          tail[1] <- tail[1] + 1
        } else {
          tail[1] <- tail[1] - 1
        }
        if (head[2] > tail[2]) {
          tail[2] <- tail[2] + 1
        } else {
          tail[2] <- tail[2] - 1
        }
      } else {
        if (head[1] > tail[1]) {
          tail[1] <- tail[1] + 1
        } else if (head[1] < tail[1]) {
          tail[1] <- tail[1] - 1
        }
        if (head[2] > tail[2]) {
          tail[2] <- tail[2] + 1
        } else if (head[2] < tail[2]) {
          tail[2] <- tail[2] - 1
        }
      }
    }
    
    visited <- rbind(visited, tail)
  }
}

print(dim(unique(visited))[1])
