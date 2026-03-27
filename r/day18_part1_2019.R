
options(scipen = 999)

solve <- function() {
  if (!file.exists("input.txt")) return()
  grid_lines <- readLines("input.txt", warn = FALSE)
  grid_lines <- grid_lines[nchar(grid_lines) > 0]
  if (length(grid_lines) == 0) return()
  
  nr <- length(grid_lines)
  nc <- nchar(grid_lines[1])
  m <- matrix(unlist(strsplit(grid_lines, "")), nrow = nr, byrow = TRUE)
  
  start_pos <- which(m == "@", arr.ind = TRUE)
  if (nrow(start_pos) == 0) return()
  start_r <- start_pos[1, 1]
  start_c <- start_pos[1, 2]
  m[start_r, start_c] <- "."
  
  keys_chars <- sort(unique(m[m %in% letters]))
  num_keys <- length(keys_chars)
  
  if (num_keys == 0) {
    cat("0\n")
    return()
  }
  
  key_map <- setNames(seq_along(keys_chars) - 1, keys_chars)
  target_mask <- bitwShiftL(1, num_keys) - 1
  
  cell_val <- matrix(0, nrow = nr, ncol = nc)
  for (i in 1:nr) {
    for (j in 1:nc) {
      char <- m[i, j]
      if (char == "#") {
        cell_val[i, j] <- NA
      } else if (char %in% letters) {
        if (char %in% keys_chars) cell_val[i, j] <- key_map[char] + 1
      } else if (char %in% LETTERS) {
        lc <- tolower(char)
        if (lc %in% keys_chars) cell_val[i, j] <- -(key_map[lc] + 1)
        else cell_val[i, j] <- NA
      }
    }
  }
  
  visited <- new.env(hash = TRUE)
  q <- list(c(start_r, start_c, 0))
  visited[[paste(0, start_r, start_c, sep = "_")]] <- TRUE
  
  dr <- c(0, 0, 1, -1)
  dc <- c(1, -1, 0, 0)
  steps <- 0
  
  while (length(q) > 0) {
    steps <- steps + 1
    next_q <- vector("list", length(q) * 4)
    count <- 0
    for (state in q) {
      for (i in 1:4) {
        nrp <- state[1] + dr[i]
        ncp <- state[2] + dc[i]
        
        if (nrp < 1 || nrp > nr || ncp < 1 || ncp > nc) next
        v <- cell_val[nrp, ncp]
        if (is.na(v)) next
        
        nk <- state[3]
        if (v < 0) {
          if (bitwAnd(nk, bitwShiftL(1, -v - 1)) == 0) next
        } else if (v > 0) {
          nk <- bitwOr(nk, bitwShiftL(1, v - 1))
          if (nk == target_mask) {
            cat(steps, "\n")
            return()
          }
        }
        
        s_key <- paste(nk, nrp, ncp, sep = "_")
        if (is.null(visited[[s_key]])) {
          visited[[s_key]] <- TRUE
          count <- count + 1
          next_q[[count]] <- c(nrp, ncp, nk)
        }
      }
    }
    q <- next_q[seq_len(count)]
  }
  cat("-1\n")
}

solve()
