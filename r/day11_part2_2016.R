
get_key <- function(elev, pairs, n_mats, idx_g, idx_m) {
  p_val <- sort((pairs[idx_g] - 1) * 4 + (pairs[idx_m] - 1))
  res <- elev - 1
  for (v in p_val) res <- res * 16 + v
  res
}

decode_key <- function(key, n_mats) {
  pairs <- numeric(2 * n_mats)
  for (i in n_mats:1) {
    v <- key %% 16
    pairs[2 * i] <- (v %% 4) + 1
    pairs[2 * i - 1] <- floor(v / 4) + 1
    key <- floor(key / 16)
  }
  list(elev = key + 1, pairs = pairs)
}

is_valid <- function(f1, f2, pairs, idx_g, idx_m) {
  for (f in c(f1, f2)) {
    if (any(pairs[idx_g] == f) && any(pairs[idx_m] == f & pairs[idx_g] != f)) return(FALSE)
  }
  TRUE
}

solve <- function() {
  lines <- readLines("input.txt")
  materials <- list()
  for (i in 1:4) {
    gs <- regmatches(lines[i], gregexpr("[a-z]+ generator", lines[i]))[[1]]
    for (g in gs) {
      name <- sub(" generator", "", g)
      if (is.null(materials[[name]])) materials[[name]] <- numeric(2)
      materials[[name]][1] <- i
    }
    cs <- regmatches(lines[i], gregexpr("[a-z]+-compatible microchip", lines[i]))[[1]]
    for (c in cs) {
      name <- sub("-compatible microchip", "", c)
      if (is.null(materials[[name]])) materials[[name]] <- numeric(2)
      materials[[name]][2] <- i
    }
  }
  
  pairs <- as.numeric(unlist(materials))
  pairs <- c(pairs, 1, 1, 1, 1)
  n_mats <- length(pairs) / 2
  idx_g <- seq(1, 2 * n_mats, 2)
  idx_m <- seq(2, 2 * n_mats, 2)
  
  initial_key <- get_key(1, pairs, n_mats, idx_g, idx_m)
  goal_key <- 3
  for (i in 1:n_mats) goal_key <- goal_key * 16 + 15
  if (initial_key == goal_key) return(0)
  
  visited <- new.env(hash = TRUE, parent = emptyenv())
  visited[[as.character(initial_key)]] <- TRUE
  curr_level <- initial_key
  depth <- 0
  
  while (length(curr_level) > 0) {
    next_level <- numeric(1000000)
    nl_ptr <- 0
    for (key in curr_level) {
      state <- decode_key(key, n_mats)
      elev <- state$elev
      pairs <- state$pairs
      indices <- which(pairs == elev)
      n_idx <- length(indices)
      
      for (dir in c(1, -1)) {
        next_elev <- elev + dir
        if (next_elev < 1 || next_elev > 4) next
        if (dir == -1) {
          empty_below <- TRUE
          for (f in 1:(elev - 1)) if (any(pairs == f)) { empty_below <- FALSE; break }
          if (empty_below) next
        }
        
        # Priority: Up moves 2 items, Down moves 1
        m_list <- list()
        if (dir == 1 && n_idx >= 2) {
          for (i in 1:(n_idx - 1)) for (j in (i + 1):n_idx) m_list[[length(m_list) + 1]] <- c(indices[i], indices[j])
        }
        for (i in 1:n_idx) m_list[[length(m_list) + 1]] <- indices[i]
        if (dir == -1 && n_idx >= 2) {
          for (i in 1:(n_idx - 1)) for (j in (i + 1):n_idx) m_list[[length(m_list) + 1]] <- c(indices[i], indices[j])
        }
        
        for (mv in m_list) {
          new_pairs <- pairs
          new_pairs[mv] <- next_elev
          if (is_valid(elev, next_elev, new_pairs, idx_g, idx_m)) {
            new_key <- get_key(next_elev, new_pairs, n_mats, idx_g, idx_m)
            if (new_key == goal_key) return(depth + 1)
            s_key <- as.character(new_key)
            if (is.null(visited[[s_key]])) {
              visited[[s_key]] <- TRUE
              nl_ptr <- nl_ptr + 1
              next_level[nl_ptr] <- new_key
            }
          }
        }
      }
    }
    curr_level <- next_level[1:nl_ptr]
    depth <- depth + 1
  }
}

cat(solve(), "\n")
