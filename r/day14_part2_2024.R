
parse_line <- function(line) {
  parts <- strsplit(line, " ")[[1]]
  p_coords <- as.integer(strsplit(gsub("[p=v]", "", parts[1]), ",")[[1]])
  v_coords <- as.integer(strsplit(gsub("[p=v]", "", parts[2]), ",")[[1]])
  list(x = p_coords[1], y = p_coords[2], vx = v_coords[1], vy = v_coords[2])
}

mod <- function(a, b) {
  (a %% b + b) %% b
}

move_robots <- function(robots, size_x, size_y) {
  for (i in seq_along(robots)) {
    robots[[i]]$x <- mod(robots[[i]]$x + robots[[i]]$vx, size_x)
    robots[[i]]$y <- mod(robots[[i]]$y + robots[[i]]$vy, size_y)
  }
  robots
}

count_quadrants <- function(robots, size_x, size_y) {
  counts <- rep(0, 4)
  center_x <- size_x / 2
  center_y <- size_y / 2
  
  for (robot in robots) {
    x <- robot$x
    y <- robot$y
    if (x < center_x) {
      if (y < center_y) {
        counts[1] <- counts[1] + 1
      } else if (y > center_y) {
        counts[2] <- counts[2] + 1
      }
    } else if (x > center_x) {
      if (y < center_y) {
        counts[3] <- counts[3] + 1
      } else if (y > center_y) {
        counts[4] <- counts[4] + 1
      }
    }
  }
  counts
}

has_no_overlaps <- function(robots) {
  positions <- sapply(robots, function(r) paste(r$x, r$y))
  length(unique(positions)) == length(positions)
}

draw_grid <- function(robots, size_x, size_y) {
  grid <- matrix(".", nrow = size_y, ncol = size_x)
  for (robot in robots) {
    grid[robot$y + 1, robot$x + 1] <- "#"
  }
  apply(grid, 1, paste, collapse = "")
}

main <- function() {
  size_x <- 101
  size_y <- 103
  
  lines <- readLines("input.txt")
  lines <- lines[lines != ""]
  robots <- lapply(lines, parse_line)
  
  robots_part1 <- robots
  for (n in 1:100) {
    robots_part1 <- move_robots(robots_part1, size_x, size_y)
  }
  
  counts <- count_quadrants(robots_part1, size_x, size_y)
  safety_factor <- prod(counts)
  cat("Part 1 - Safety Factor after 100 seconds:", safety_factor, "\n")
  
  robots_part2 <- robots
  seconds <- 0
  while (TRUE) {
    if (has_no_overlaps(robots_part2)) {
      break
    }
    robots_part2 <- move_robots(robots_part2, size_x, size_y)
    seconds <- seconds + 1
    if (seconds > 1000000) {
      cat("Exceeded maximum iterations without finding a unique position configuration.\n")
      return()
    }
  }
  cat("Part 2 - Fewest seconds to display Easter egg:", seconds, "\n")
  cat("Final positions of robots:\n")
  grid_output <- draw_grid(robots_part2, size_x, size_y)
  cat(paste(grid_output, collapse = "\n"), "\n")
}

main()
