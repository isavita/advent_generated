
get_dist <- function(goal_p, start_p, target_p, w, h, walls) {
  if (all(start_p == target_p)) return(0)
  dist <- matrix(-1, w, h)
  dist[start_p[1] + 1, start_p[2] + 1] <- 0
  q <- list(start_p)
  head <- 1
  while (head <= length(q)) {
    curr <- q[[head]]
    head <- head + 1
    d <- dist[curr[1] + 1, curr[2] + 1]
    for (m in list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))) {
      nx <- curr[1] + m[1]
      ny <- curr[2] + m[2]
      if (nx >= 0 && nx < w && ny >= 0 && ny < h && 
          !(nx == goal_p[1] && ny == goal_p[2]) && 
          !walls[nx + 1, ny + 1] && dist[nx + 1, ny + 1] == -1) {
        dist[nx + 1, ny + 1] <- d + 1
        if (nx == target_p[1] && ny == target_p[2]) return(d + 1)
        q[[length(q) + 1]] <- c(nx, ny)
      }
    }
  }
  return(-1)
}

lines <- readLines("input.txt")
lines <- lines[grep("^/dev/grid/node-x", lines)]
parts <- do.call(rbind, strsplit(trimws(lines), "\\s+"))
raw_coords <- regmatches(parts[, 1], regexec("x(\\d+)-y(\\d+)", parts[, 1]))
coords <- do.call(rbind, lapply(raw_coords, function(x) as.numeric(x[2:3])))
used <- as.numeric(sub("T", "", parts[, 3]))

width <- max(coords[, 1]) + 1
height <- max(coords[, 2]) + 1

grid_used <- matrix(0, width, height)
for (i in 1:nrow(coords)) {
  grid_used[coords[i, 1] + 1, coords[i, 2] + 1] <- used[i]
}

walls <- grid_used > 400
hole_pos <- coords[which(used == 0), ]
goal_pos <- c(width - 1, 0)
moves_sum <- 0

while (goal_pos[1] != 0 || goal_pos[2] != 0) {
  target_hole <- c(goal_pos[1] - 1, goal_pos[2])
  m <- get_dist(goal_pos, hole_pos, target_hole, width, height, walls)
  if (m == -1) break
  moves_sum <- moves_sum + m + 1
  hole_pos <- goal_pos
  goal_pos <- target_hole
}

cat(moves_sum, "\n")
