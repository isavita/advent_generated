
options(scipen = 999)

solve <- function() {
  if (!file.exists("input.txt")) return()
  
  lines <- readLines("input.txt")
  matches <- gregexpr("-?\\d+", lines)
  coords <- lapply(regmatches(lines, matches), as.numeric)
  mat <- do.call(rbind, coords)
  
  sx <- mat[, 1]
  sy <- mat[, 2]
  bx <- mat[, 3]
  by <- mat[, 4]
  dist_vec <- abs(sx - bx) + abs(sy - by)
  
  max_coord <- 4000000
  
  for (y in 0:max_coord) {
    dx_max <- dist_vec - abs(sy - y)
    valid_idx <- which(dx_max >= 0)
    
    if (length(valid_idx) == 0) {
      cat(y, "\n")
      return()
    }
    
    starts <- sx[valid_idx] - dx_max[valid_idx]
    ends <- sx[valid_idx] + dx_max[valid_idx]
    
    ord <- order(starts)
    s_sorted <- starts[ord]
    e_sorted <- ends[ord]
    
    if (s_sorted[1] > 0) {
      cat(as.numeric(y), "\n")
      return()
    }
    
    curr_e <- e_sorted[1]
    n_intervals <- length(s_sorted)
    
    if (n_intervals > 1) {
      for (i in 2:n_intervals) {
        if (s_sorted[i] > curr_e + 1) {
          res_x <- curr_e + 1
          if (res_x <= max_coord) {
            cat(as.numeric(res_x) * 4000000 + y, "\n")
            return()
          }
        }
        if (e_sorted[i] > curr_e) {
          curr_e <- e_sorted[i]
        }
        if (curr_e >= max_coord) break
      }
    }
    
    if (curr_e < max_coord) {
      cat(as.numeric(curr_e + 1) * 4000000 + y, "\n")
      return()
    }
  }
}

solve()
