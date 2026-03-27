
options(scipen = 999)

input <- readLines("input.txt", warn = FALSE)
jet_pattern <- strsplit(trimws(input[1]), "")[[1]]
jet_count <- length(jet_pattern)

rocks <- list(
  matrix(c(0,0, 1,0, 2,0, 3,0), ncol = 2, byrow = TRUE),
  matrix(c(1,0, 0,1, 1,1, 2,1, 1,2), ncol = 2, byrow = TRUE),
  matrix(c(0,0, 1,0, 2,0, 2,1, 2,2), ncol = 2, byrow = TRUE),
  matrix(c(0,0, 0,1, 0,2, 0,3), ncol = 2, byrow = TRUE),
  matrix(c(0,0, 1,0, 0,1, 1,1), ncol = 2, byrow = TRUE)
)

chamber <- integer(1000000)
highest_y <- 0
jet_ptr <- 1
rock_count <- 0
total_rocks <- 1000000000000
history <- new.env(hash = TRUE)
added_h <- 0
cycle_found <- FALSE

get_profile <- function(chamber, highest_y) {
  vapply(0:6, function(x) {
    for (d in 0:30) {
      y <- highest_y - d
      if (y <= 0) return(100)
      if (bitwAnd(chamber[y], bitwShiftL(1, x)) != 0) return(d)
    }
    100
  }, numeric(1))
}

can_move <- function(pts, dx, dy, chamber) {
  nx <- pts[, 1] + dx
  ny <- pts[, 2] + dy
  if (any(nx < 0 | nx > 6 | ny <= 0)) return(FALSE)
  if (any(bitwAnd(chamber[ny], bitwShiftL(1, nx)) != 0)) return(FALSE)
  TRUE
}

while (rock_count < total_rocks) {
  rock_idx <- (rock_count %% 5) + 1
  
  if (!cycle_found && rock_count > jet_count) {
    prof <- get_profile(chamber, highest_y)
    key <- paste(rock_idx, jet_ptr, paste(prof, collapse = ","), sep = "_")
    if (exists(key, envir = history)) {
      prev <- get(key, envir = history)
      cycle_len <- rock_count - prev[1]
      cycle_h <- highest_y - prev[2]
      num_cycles <- floor((total_rocks - rock_count) / cycle_len)
      added_h <- num_cycles * cycle_h
      rock_count <- rock_count + num_cycles * cycle_len
      cycle_found <- TRUE
      if (rock_count >= total_rocks) break
    } else {
      assign(key, c(rock_count, highest_y), envir = history)
    }
  }

  pts <- rocks[[rock_idx]]
  pts[, 1] <- pts[, 1] + 2
  pts[, 2] <- pts[, 2] + highest_y + 4

  repeat {
    move <- jet_pattern[jet_ptr]
    dx <- if (move == ">") 1 else -1
    jet_ptr <- (jet_ptr %% jet_count) + 1
    if (can_move(pts, dx, 0, chamber)) pts[, 1] <- pts[, 1] + dx
    
    if (can_move(pts, 0, -1, chamber)) {
      pts[, 2] <- pts[, 2] - 1
    } else {
      for (i in 1:nrow(pts)) {
        y <- pts[i, 2]
        chamber[y] <- bitwOr(chamber[y], bitwShiftL(1, pts[i, 1]))
        if (y > highest_y) highest_y <- y
      }
      break
    }
  }
  rock_count <- rock_count + 1
}

cat(sprintf("%.0f\n", highest_y + added_h))
