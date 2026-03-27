
options(expressions = 100000)

normalize_shape <- function(p) {
  if (is.null(p) || nrow(p) == 0) return(NULL)
  p[,1] <- p[,1] - min(p[,1])
  p[,2] <- p[,2] - min(p[,2])
  p <- p[order(p[,1], p[,2]), , drop=FALSE]
  p[,1] <- p[,1] - p[1,1]
  p[,2] <- p[,2] - p[1,2]
  p
}

generate_variations <- function(p) {
  if (is.null(p)) return(list())
  vars <- list()
  curr <- p
  for (i in 1:4) {
    for (f in 1:2) {
      nv <- normalize_shape(curr)
      is_new <- TRUE
      for (v in vars) {
        if (nrow(v) == nrow(nv) && all(v == nv)) {
          is_new <- FALSE; break
        }
      }
      if (is_new) vars[[length(vars)+1]] <- nv
      curr[,2] <- -curr[,2]
    }
    tmp <- curr[,1]
    curr[,1] <- curr[,2]
    curr[,2] <- -tmp
  }
  vars
}

check_islands <- function(width, height, slack_id, min_real_size, env) {
  avail_slack <- env$counts[slack_id]
  vis <- env$grid
  free_cells <- which(vis == 0)
  if (length(free_cells) == 0) return(TRUE)
  
  q <- integer(length(vis))
  for (idx in free_cells) {
    if (vis[idx] == 0) {
      q[1] <- idx
      vis[idx] <- 1
      head <- 1
      tail <- 1
      while(head <= tail) {
        curr <- q[head]
        head <- head + 1
        curr_r <- (curr - 1) %/% width + 1
        curr_c <- (curr - 1) %% width + 1
        
        if (curr_r > 1) {
          n <- curr - width
          if (vis[n] == 0) { vis[n] <- 1; tail <- tail + 1; q[tail] <- n }
        }
        if (curr_r < height) {
          n <- curr + width
          if (vis[n] == 0) { vis[n] <- 1; tail <- tail + 1; q[tail] <- n }
        }
        if (curr_c > 1) {
          n <- curr - 1
          if (vis[n] == 0) { vis[n] <- 1; tail <- tail + 1; q[tail] <- n }
        }
        if (curr_c < width) {
          n <- curr + 1
          if (vis[n] == 0) { vis[n] <- 1; tail <- tail + 1; q[tail] <- n }
        }
      }
      size <- tail
      if (size < min_real_size) {
        avail_slack <- avail_slack - size
        if (avail_slack < 0) return(FALSE)
      }
    }
  }
  TRUE
}

solve_rec <- function(width, height, ids, slack_id, shape_sizes, variations, env) {
  empty <- which(env$grid == 0)[1]
  if (is.na(empty)) return(TRUE)
  
  rem_real <- ids[ids != slack_id & env$counts[ids] > 0]
  if (length(rem_real) > 0) {
    min_real <- min(shape_sizes[rem_real])
    if (!check_islands(width, height, slack_id, min_real, env)) return(FALSE)
  } else {
    return(TRUE)
  }
  
  r <- (empty - 1) %/% width + 1
  c <- (empty - 1) %% width + 1
  
  for (id in ids) {
    if (env$counts[id] > 0) {
      env$counts[id] <- env$counts[id] - 1
      for (v in variations[[id]]) {
        idx <- (r + v[,1] - 1) * width + (c + v[,2])
        if (all(r + v[,1] >= 1 & r + v[,1] <= height & c + v[,2] >= 1 & c + v[,2] <= width) && 
            all(env$grid[idx] == 0)) {
          env$grid[idx] <- 1
          if (solve_rec(width, height, ids, slack_id, shape_sizes, variations, env)) return(TRUE)
          env$grid[idx] <- 0
        }
      }
      env$counts[id] <- env$counts[id] + 1
    }
  }
  FALSE
}

input_lines <- readLines("input.txt", warn = FALSE)
input_lines <- trimws(input_lines)
input_lines <- input_lines[nchar(input_lines) > 0]

shapes <- list()
max_id <- -1
mode <- "SHAPES"
curr_id <- -1
curr_shape_pts <- list()
curr_shape_rows <- 0
region_lines <- character()

for (line in input_lines) {
  if (mode == "SHAPES") {
    if (grepl("^\\d+:$", line)) {
      if (curr_id != -1) shapes[[curr_id + 1]] <- do.call(rbind, curr_shape_pts)
      curr_id <- as.integer(sub(":", "", line))
      max_id <- max(max_id, curr_id)
      curr_shape_pts <- list()
      curr_shape_rows <- 0
    } else if (grepl("x.*:", line)) {
      if (curr_id != -1) shapes[[curr_id + 1]] <- do.call(rbind, curr_shape_pts)
      mode <- "REGIONS"
      region_lines <- c(region_lines, line)
    } else {
      curr_shape_rows <- curr_shape_rows + 1
      chars <- strsplit(line, "")[[1]]
      for (cc in seq_along(chars)) {
        if (chars[cc] == "#") curr_shape_pts[[length(curr_shape_pts) + 1]] <- c(curr_shape_rows, cc)
      }
    }
  } else {
    region_lines <- c(region_lines, line)
  }
}
if (curr_id != -1 && mode == "SHAPES") shapes[[curr_id + 1]] <- do.call(rbind, curr_shape_pts)

slack_id <- max_id + 2
shapes[[slack_id]] <- matrix(c(1,1), nrow=1)
shape_sizes <- sapply(shapes, function(s) if(is.null(s)) 0 else nrow(s))
variations <- lapply(shapes, function(s) generate_variations(normalize_shape(s)))

solved_count <- 0
for (line in region_lines) {
  parts <- strsplit(line, ":")[[1]]
  wh <- as.integer(strsplit(trimws(parts[1]), "x")[[1]])
  w <- wh[1]; h <- wh[2]; gs <- w * h
  
  p_counts <- rep(0, slack_id)
  c_vals <- as.integer(strsplit(trimws(parts[2]), "\\s+")[[1]])
  for (idx in seq_along(c_vals)) if (idx < slack_id) p_counts[idx] <- c_vals[idx]
  
  t_area <- sum(p_counts * shape_sizes)
  if (t_area > gs) next
  p_counts[slack_id] <- gs - t_area
  
  ids <- which(p_counts > 0)
  ids <- ids[order(shape_sizes[ids], decreasing = TRUE)]
  env <- new.env(); env$grid <- rep(0, gs); env$counts <- p_counts
  if (solve_rec(w, h, ids, slack_id, shape_sizes, variations, env)) solved_count <- solved_count + 1
}

cat(sprintf("Number of regions that fit all presents: %d\n", solved_count))
