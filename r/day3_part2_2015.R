# Read input from file
input <- readLines("input.txt")

# Define movement vectors
movements <- list(
  "^" = c(0, 1),
  "v" = c(0, -1),
  ">" = c(1, 0),
  "<" = c(-1, 0)
)

# Part 1: Count houses visited by Santa
santa_position <- c(0, 0)
visited_houses <- list(santa_position)
for (move in strsplit(input[[1]], "")[[1]]) {
  santa_position <- santa_position + movements[[move]]
  visited_houses[[length(visited_houses) + 1]] <- santa_position
}
part1_answer <- length(unique(visited_houses))

# Part 2: Count houses visited by Santa and Robo-Santa
santa_position <- c(0, 0)
robo_santa_position <- c(0, 0)
visited_houses <- list(santa_position, robo_santa_position)
for (i in seq_along(strsplit(input[[1]], "")[[1]])) {
  move <- strsplit(input[[1]], "")[[1]][i]
  if (i %% 2 == 1) {
    santa_position <- santa_position + movements[[move]]
    visited_houses[[length(visited_houses) + 1]] <- santa_position
  } else {
    robo_santa_position <- robo_santa_position + movements[[move]]
    visited_houses[[length(visited_houses) + 1]] <- robo_santa_position
  }
}
part2_answer <- length(unique(visited_houses))

# Print answers
cat("Part 1 answer:", part1_answer, "\n")
cat("Part 2 answer:", part2_answer, "\n")