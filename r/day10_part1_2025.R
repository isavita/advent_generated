
generate_combos <- function(n) {
  if (n <= 0) return(matrix(0, nrow = 1, ncol = 0))
  m <- 2^n
  res <- matrix(0L, nrow = m, ncol = n)
  for (j in 1:n) {
    res[, j] <- rep(rep(c(0L, 1L), each = 2^(j - 1)), length.out = m)
  }
  res
}

solve <- function() {
  if (!file.exists("input.txt")) return()
  lines <- readLines("input.txt", warn = FALSE)
  total_presses <- 0
  
  for (line in lines) {
    if (!grepl("\\[", line)) next
    
    target_part <- regmatches(line, regexpr("\\[[.#]+\\]", line))
    if (length(target_part) == 0) next
    target_str <- substr(target_part, 2, nchar(target_part) - 1)
    target_vec <- as.integer(strsplit(target_str, "")[[1]] == "#")
    R_val <- length(target_vec)
    
    btn_matches <- regmatches(line, gregexpr("\\([0-9, ]+\\)", line))[[1]]
    C_val <- length(btn_matches)
    
    mat <- matrix(0L, nrow = R_val, ncol = C_val + 1)
    if (C_val > 0) {
      for (j in seq_len(C_val)) {
        nums_str <- gsub("[\\(\\)]", "", btn_matches[j])
        if (nchar(gsub(" ", "", nums_str)) > 0) {
          nums <- as.integer(strsplit(nums_str, ",")[[1]])
          mat[nums + 1, j] <- 1L
        }
      }
    }
    mat[, C_val + 1] <- target_vec
    
    pivot_row <- 1
    pivot_cols <- integer(0)
    for (j in seq_len(C_val)) {
      if (pivot_row > R_val) break
      sel <- which(mat[pivot_row:R_val, j] == 1L)
      if (length(sel) == 0) next
      sel <- sel[1] + pivot_row - 1
      
      tmp <- mat[pivot_row, ]
      mat[pivot_row, ] <- mat[sel, ]
      mat[sel, ] <- tmp
      
      others <- which(mat[, j] == 1L)
      for (r in others) {
        if (r != pivot_row) {
          mat[r, ] <- bitwXor(mat[r, ], mat[pivot_row, ])
        }
      }
      pivot_cols <- c(pivot_cols, j)
      pivot_row <- pivot_row + 1
    }
    
    if (pivot_row <= R_val && any(mat[pivot_row:R_val, C_val + 1] == 1L)) next
    
    free_vars <- setdiff(seq_len(C_val), pivot_cols)
    n_free <- length(free_vars)
    
    A <- mat[seq_along(pivot_cols), free_vars, drop = FALSE]
    b <- mat[seq_along(pivot_cols), C_val + 1]
    
    if (n_free == 0) {
      min_w <- sum(b)
    } else {
      combos <- generate_combos(n_free)
      if (length(b) == 0) {
        min_w <- min(rowSums(combos))
      } else {
        pivots_matrix <- (matrix(b, nrow = nrow(combos), ncol = length(b), byrow = TRUE) + combos %*% t(A)) %% 2
        min_w <- min(rowSums(combos) + rowSums(pivots_matrix))
      }
    }
    total_presses <- total_presses + min_w
  }
  cat(sprintf("%d\n", as.integer(total_presses)))
}

solve()
