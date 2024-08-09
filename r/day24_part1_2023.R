parse_input <- function(input) {
  points <- lapply(input, function(line) {
    as.numeric(unlist(strsplit(gsub(" @ ", ",", line), ",")))
  })
  lapply(points, function(p) list(pos = p[1:3], vel = p[4:6]))
}

is_intersecting_2d <- function(p1, p2) {
  det <- p1$vel[1] * p2$vel[2] - p2$vel[1] * p1$vel[2]
  if (det == 0) return(list(intersecting = FALSE))
  t1 <- (p2$vel[2] * (p2$pos[1] - p1$pos[1]) - p2$vel[1] * (p2$pos[2] - p1$pos[2])) / det
  t2 <- (p1$vel[2] * (p2$pos[1] - p1$pos[1]) - p1$vel[1] * (p2$pos[2] - p1$pos[2])) / det
  coord <- c(p1$pos[1] + p1$vel[1] * t1, p1$pos[2] + p1$vel[2] * t1, 0)
  list(intersecting = TRUE, coord = coord, t1 = t1, t2 = t2)
}

solve <- function(input, min, max) {
  points <- parse_input(input)
  cnt <- 0
  for (i in seq_along(points)) {
    for (j in seq_len(i - 1)) {
      result <- is_intersecting_2d(points[[i]], points[[j]])
      if (result$intersecting) {
        coord <- result$coord
        if (min <= coord[1] && coord[1] <= max && min <= coord[2] && coord[2] <= max &&
            result$t1 >= 0 && result$t2 >= 0) {
          cnt <- cnt + 1
        }
      }
    }
  }
  cnt
}

read_file <- function(file_name) {
  input <- readLines(file_name)
  trimws(input)
}

main <- function() {
  input <- read_file("input.txt")
  print(solve(input, 200000000000000, 400000000000000))
}

main()