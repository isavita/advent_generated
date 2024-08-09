sensors <- list()

read_input <- function(file) {
  readLines(file)
}

manhattan <- function(p, q) {
  abs(p[1] - q[1]) + abs(p[2] - q[2])
}

impossible <- function(sensors, y) {
  pts <- integer()
  for (s in sensors) {
    dist <- s$dist - abs(s$pos[2] - y)
    if (dist >= 0) {
      pts <- unique(c(pts, (s$pos[1] + 0:dist), (s$pos[1] - 0:dist)))
    }
  }
  for (s in sensors) {
    if (s$beacon[2] == y) {
      pts <- pts[pts != s$beacon[1]]
    }
  }
  length(pts)
}

input <- read_input("input.txt")
for (line in input) {
  s <- list()
  matches <- regmatches(line, gregexpr("-?\\d+", line))
  s$pos <- as.integer(matches[[1]][1:2])
  s$beacon <- as.integer(matches[[1]][3:4])
  s$dist <- manhattan(s$pos, s$beacon)
  sensors <- append(sensors, list(s))
}

print(impossible(sensors, 2000000))