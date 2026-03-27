rref <- function(M) {
  n <- nrow(M); m <- ncol(M)
  p_row <- 1; p_cols <- rep(0, n); p_idx <- c()
  if (m > 1) {
    for (j in 1:(m - 1)) {
      if (p_row > n) break
      sel <- which(abs(M[p_row:n, j]) > 1e-7)
      if (length(sel) == 0) next
      sel <- sel[1] + p_row - 1
      tmp <- M[p_row, ]; M[p_row, ] <- M[sel, ]; M[sel, ] <- tmp
      M[p_row, ] <- M[p_row, ] / M[p_row, j]
      for (i in 1:n) if (i != p_row) M[i, ] <- M[i, ] - M[i, j] * M[p_row, ]
      p_idx <- c(p_idx, j); p_cols[p_row] <- j; p_row <- p_row + 1
    }
  }
  list(M = M, pivots = p_idx, pivot_cols = p_cols)
}

solve_case <- function(line) {
  btns_s <- regmatches(line, gregexpr("\\([0-9,]+\\)", line))[[1]]
  btns <- lapply(btns_s, function(s) as.numeric(strsplit(gsub("[()]", "", s), ",")[[1]]))
  trg_s <- regmatches(line, gregexpr("\\{[0-9,]+\\}", line))[[1]]
  target <- as.numeric(strsplit(gsub("[{}]", "", trg_s), ",")[[1]])
  
  num_btns <- length(btns); num_ctrs <- length(target)
  if (num_btns == 0) return(-1)
  A <- matrix(0, num_ctrs, num_btns)
  for (i in 1:num_btns) {
    v <- btns[[i]][btns[[i]] < num_ctrs]
    if (length(v) > 0) A[v + 1, i] <- 1
  }
  
  max_p <- numeric(num_btns)
  for (i in 1:num_btns) {
    v <- btns[[i]][btns[[i]] < num_ctrs]
    max_p[i] <- if (length(v) > 0) min(target[v + 1]) else 0
  }
  
  res <- rref(cbind(A, target))
  M <- res$M; p_cols <- res$pivot_cols; pivots <- res$pivots
  free_v <- setdiff(1:num_btns, pivots)
  if (length(free_v) > 0) free_v <- free_v[order(max_p[free_v])]
  
  num_f <- length(free_v); f_vals <- numeric(num_f); best <- Inf
  
  dfs <- function(idx, cur_s) {
    if (cur_s >= best) return()
    if (idx > num_f) {
      x <- numeric(num_btns)
      if (num_f > 0) x[free_v] <- f_vals
      for (r in 1:num_ctrs) {
        pc <- p_cols[r]
        if (pc == 0) { if (abs(M[r, num_btns+1]) > 1e-7) return() else next }
        val <- M[r, num_btns+1] - if (num_f > 0) sum(M[r, free_v] * x[free_v]) else 0
        iv <- round(val)
        if (abs(val - iv) > 1e-7 || iv < 0 || iv > max_p[pc]) return()
        x[pc] <- iv
      }
      sx <- sum(x)
      if (sx > 0 && sx < best) best <<- sx
      return()
    }
    for (v in 0:max_p[free_v[idx]]) {
      f_vals[idx] <<- v
      dfs(idx + 1, cur_s + v)
    }
  }
  
  dfs(1, 0)
  if (best == Inf) -1 else best
}

lines <- readLines("input.txt", warn = FALSE)
ans <- 0
for (l in lines) if (nchar(l) > 0) {
  r <- solve_case(l)
  if (r > 0) ans <- ans + r
}
cat(ans, "\n")