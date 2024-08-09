depth_target <- readLines("input.txt")
depth <- as.integer(strsplit(depth_target[1], " ")[[1]][2])
coords <- as.integer(unlist(strsplit(strsplit(depth_target[2], " ")[[1]][2], ",")))

make_cave_system <- function(depth, target) {
  cave <- matrix(0, nrow = target[2] + 1, ncol = target[1] + 1)
  for (y in 0:target[2]) {
    for (x in 0:target[1]) {
      geologic_index <- ifelse((x == 0 && y == 0) || (x == target[1] && y == target[2]), 0,
                               ifelse(y == 0, x * 16807,
                               ifelse(x == 0, y * 48271,
                               cave[y + 1, x] * cave[y, x + 1])))
      cave[y + 1, x + 1] <- (geologic_index + depth) %% 20183
    }
  }
  cave
}

calculate_risk_level <- function(cave, target) {
  sum(cave[1:(target[2] + 1), 1:(target[1] + 1)] %% 3)
}

cave <- make_cave_system(depth, coords)
risk_level <- calculate_risk_level(cave, coords)
print(risk_level)