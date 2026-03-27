
dy <- c(-1, 0, 0, 1)
dx <- c(0, -1, 1, 0)

bfs <- function(sy, sx, grid, rows, cols) {
  dist <- matrix(NA, rows, cols)
  dist[sy, sx] <- 0
  q_y <- integer(rows * cols)
  q_x <- integer(rows * cols)
  q_y[1] <- sy
  q_x[1] <- sx
  head <- 1
  tail <- 2
  while (head < tail) {
    cy <- q_y[head]
    cx <- q_x[head]
    head <- head + 1
    d_next <- dist[cy, cx] + 1
    for (i in 1:4) {
      ny <- cy + dy[i]
      nx <- cx + dx[i]
      if (grid[ny, nx] == "." && is.na(dist[ny, nx])) {
        dist[ny, nx] <- d_next
        q_y[tail] <- ny
        q_x[tail] <- nx
        tail <- tail + 1
      }
    }
  }
  dist
}

find_attack_target <- function(u, units, unit_map) {
  best_v_idx <- NULL
  min_hp <- Inf
  for (i in 1:4) {
    ny <- u$y + dy[i]
    nx <- u$x + dx[i]
    v_idx <- unit_map[ny, nx]
    if (v_idx > 0) {
      v <- units[[v_idx]]
      if (v$kind != u$kind) {
        if (v$hp < min_hp) {
          min_hp <- v$hp
          best_v_idx <- v_idx
        }
      }
    }
  }
  best_v_idx
}

find_move <- function(u, units, grid, rows, cols, enemy_kind) {
  dist_map <- bfs(u$y, u$x, grid, rows, cols)
  min_dist <- Inf
  target_squares <- list()
  for (v in units) {
    if (v$alive && v$kind == enemy_kind) {
      for (i in 1:4) {
        ny <- v$y + dy[i]; nx <- v$x + dx[i]
        if (grid[ny, nx] == ".") {
          d <- dist_map[ny, nx]
          if (!is.na(d)) {
            if (d < min_dist) {
              min_dist <- d
              target_squares <- list(c(ny, nx))
            } else if (d == min_dist) {
              target_squares[[length(target_squares) + 1]] <- c(ny, nx)
            }
          }
        }
      }
    }
  }
  if (length(target_squares) == 0) return(NULL)
  target_df <- do.call(rbind, target_squares)
  chosen_target <- target_df[order(target_df[, 1], target_df[, 2]), , drop = FALSE][1, ]
  dist_map_back <- bfs(chosen_target[1], chosen_target[2], grid, rows, cols)
  for (i in 1:4) {
    ny <- u$y + dy[i]; nx <- u$x + dx[i]
    if (grid[ny, nx] == "." && !is.na(dist_map_back[ny, nx])) {
      if (dist_map_back[ny, nx] == min_dist - 1) return(list(y = ny, x = nx))
    }
  }
  NULL
}

simulate <- function(elf_power, initial_grid, initial_units, rows, cols) {
  grid <- initial_grid
  units <- initial_units
  num_elves <- 0
  num_goblins <- 0
  for (idx in seq_along(units)) {
    if (units[[idx]]$kind == "E") {
      units[[idx]]$power <- elf_power
      num_elves <- num_elves + 1
    } else {
      num_goblins <- num_goblins + 1
    }
  }
  unit_map <- matrix(0, rows, cols)
  for (idx in seq_along(units)) unit_map[units[[idx]]$y, units[[idx]]$x] <- idx
  rounds <- 0
  while (TRUE) {
    is_alive <- sapply(units, function(u) u$alive)
    alive_indices <- which(is_alive)
    y_coords <- sapply(units, function(u) u$y)
    x_coords <- sapply(units, function(u) u$x)
    unit_order <- alive_indices[order(y_coords[alive_indices], x_coords[alive_indices])]
    full_round <- TRUE
    for (idx in unit_order) {
      if (!units[[idx]]$alive) next
      u <- units[[idx]]
      if (u$kind == "E") {
        if (num_goblins == 0) { full_round <- FALSE; break }
        enemy_kind <- "G"
      } else {
        if (num_elves == 0) { full_round <- FALSE; break }
        enemy_kind <- "E"
      }
      target_v_idx <- find_attack_target(u, units, unit_map)
      if (is.null(target_v_idx)) {
        new_pos <- find_move(u, units, grid, rows, cols, enemy_kind)
        if (!is.null(new_pos)) {
          unit_map[u$y, u$x] <- 0
          grid[u$y, u$x] <- "."
          u$y <- new_pos$y; u$x <- new_pos$x
          units[[idx]]$y <- u$y; units[[idx]]$x <- u$x
          unit_map[u$y, u$x] <- idx
          grid[u$y, u$x] <- u$kind
          target_v_idx <- find_attack_target(u, units, unit_map)
        }
      }
      if (!is.null(target_v_idx)) {
        units[[target_v_idx]]$hp <- units[[target_v_idx]]$hp - u$power
        if (units[[target_v_idx]]$hp <= 0) {
          units[[target_v_idx]]$alive <- FALSE
          unit_map[units[[target_v_idx]]$y, units[[target_v_idx]]$x] <- 0
          grid[units[[target_v_idx]]$y, units[[target_v_idx]]$x] <- "."
          if (units[[target_v_idx]]$kind == "E") return(list(elf_died = TRUE))
          num_goblins <- num_goblins - 1
        }
      }
    }
    if (!full_round) break
    rounds <- rounds + 1
  }
  hp_sum <- sum(sapply(units, function(v) if (v$alive) v$hp else 0))
  list(outcome = rounds * hp_sum, elf_died = FALSE)
}

lines <- readLines("input.txt")
lines <- lines[lines != ""]
rows <- length(lines)
cols <- nchar(lines[1])
initial_grid <- matrix(unlist(strsplit(lines, "")), rows, cols, byrow = TRUE)
initial_units <- list()
for (r in 1:rows) {
  for (c in 1:cols) {
    if (initial_grid[r, c] %in% c("E", "G")) {
      initial_units[[length(initial_units) + 1]] <- list(
        kind = initial_grid[r, c], y = r, x = c, hp = 200, power = 3, alive = TRUE
      )
    }
  }
}

elf_power <- 4
while (TRUE) {
  res <- simulate(elf_power, initial_grid, initial_units, rows, cols)
  if (!res$elf_died) {
    cat(res$outcome, "\n")
    break
  }
  elf_power <- elf_power + 1
}
