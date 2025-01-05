
parse_input <- function(file = "input.txt") {
  lines <- readLines(file)
  size <- nchar(lines[1]) %/% 3
  map <- list()
  for (r in seq_along(lines)) {
    line <- lines[r]
    if (line == "") break
    for (c in seq_len(nchar(line))) {
      char <- substr(line, c, c)
      if (char == " ") next
      map[[paste(r - 1, c - 1)]] <- char == "#"
    }
  }
  path <- lines[r + 1]
  list(map = map, size = size, path = path)
}

parse_path <- function(path) {
  moves <- gregexpr("([RL]|\\d+)", path)[[1]]
  movements <- list()
  for (i in seq_along(moves)) {
    start <- moves[i]
    len <- attr(moves, "match.length")[i]
    token <- substr(path, start, start + len - 1)
    if (token %in% c("R", "L")) {
      movements[[length(movements) + 1]] <- list(rotate = token)
    } else {
      movements[[length(movements) + 1]] <- list(steps = as.integer(token))
    }
  }
  movements
}

rotate <- function(dir, direction) {
  if (direction == "R") {
    (dir + 1) %% 4
  } else {
    (dir - 1 + 4) %% 4
  }
}

points <- function(dir) {
  (dir + 3) %% 4
}

dirs <- list(c(-1, 0), c(0, 1), c(1, 0), c(0, -1))

cross_border <- function(n, dir, size) {
  x <- n[1]
  y <- n[2]
  
  if (x == -1 && y < 2 * size) {
    return(list(next_pos = c(y + 2 * size, x + 1), next_dir = 1))
  } else if (x == -1 && y >= 2 * size) {
    return(list(next_pos = c(x + 4 * size, y - 2 * size), next_dir = 0))
  } else if (x == size && dir == 2) {
    return(list(next_pos = c(y - size, x + size - 1), next_dir = 3))
  } else if (x == 2 * size - 1 && dir == 0) {
    return(list(next_pos = c(y + size, x - size + 1), next_dir = 1))
  } else if (x == 3 * size && dir == 2) {
    return(list(next_pos = c(y + 2 * size, x - 2 * size - 1), next_dir = 3))
  } else if (x == 4 * size) {
    return(list(next_pos = c(x - 4 * size, y + 2 * size), next_dir = 2))
  } else if (y == -1 && x < 3 * size) {
    return(list(next_pos = c(3 * size - 1 - x, y + size + 1), next_dir = 1))
  } else if (y == -1 && x >= 3 * size) {
    return(list(next_pos = c(y + 1, x - 2 * size), next_dir = 2))
  } else if (y == size - 1 && x < size) {
    return(list(next_pos = c(3 * size - 1 - x, y - size + 1), next_dir = 1))
  } else if (y == size - 1 && x >= size && dir == 3) {
    return(list(next_pos = c(y + size + 1, x - size), next_dir = 2))
  } else if (y == size && dir == 1) {
    return(list(next_pos = c(y + 2 * size - 1, x - 2 * size), next_dir = 0))
  } else if (y == 2 * size && x < 2 * size && dir == 1) {
    return(list(next_pos = c(y - size - 1, x + size), next_dir = 0))
  } else if (y == 2 * size && x >= 2 * size) {
    return(list(next_pos = c(3 * size - 1 - x, y + size - 1), next_dir = 3))
  } else if (y == 3 * size) {
    return(list(next_pos = c(3 * size - 1 - x, y - size - 1), next_dir = 3))
  }
  stop("not a border crossing")
}

walk <- function(curr, facing, map, size) {
  dir_delta <- dirs[[facing + 1]]
  next_pos <- curr + dir_delta
  
  if (paste(next_pos[1], next_pos[2]) %in% names(map)) {
    if (map[[paste(next_pos[1], next_pos[2])]]) {
      return(list(moved = FALSE, curr = curr, facing = facing))
    }
    return(list(moved = TRUE, curr = next_pos, facing = facing))
  }
  
  border_result <- cross_border(next_pos, facing, size)
  next_pos <- border_result$next_pos
  next_facing <- border_result$next_dir
  
  if (map[[paste(next_pos[1], next_pos[2])]]) {
    return(list(moved = FALSE, curr = curr, facing = facing))
  }
  
  return(list(moved = TRUE, curr = next_pos, facing = next_facing))
}

main <- function() {
  input <- parse_input()
  map <- input$map
  size <- input$size
  movements <- parse_path(input$path)
  
  curr <- c(0, size)
  facing <- 1
  
  for (mov in movements) {
    if (!is.null(mov$rotate)) {
      facing <- rotate(facing, mov$rotate)
    } else {
      for (i in seq_len(mov$steps)) {
        walk_result <- walk(curr, facing, map, size)
        if (!walk_result$moved) {
          break
        }
        curr <- walk_result$curr
        facing <- walk_result$facing
      }
    }
  }
  
  print(1000 * (curr[1] + 1) + 4 * (curr[2] + 1) + points(facing))
}

main()
