lines <- readLines("input.txt")
lines <- lines[nzchar(lines)]
grid <- do.call(rbind, strsplit(lines, ""))
H <- nrow(grid)
W <- ncol(grid)
ir <- 2:(H - 1)
ic <- 2:(W - 1)
IW <- W - 2
IH <- H - 2
walls <- grid == "#"
is_R <- grid == ">"
is_L <- grid == "<"
is_D <- grid == "v"
is_U <- grid == "^"
start_pos <- c(1, which(grid[1, ] == ".")[1])
end_pos <- c(H, which(grid[H, ] == ".")[1])

solve_segment <- function(start_node, end_node, start_time) {
  curr <- matrix(FALSE, H, W)
  curr[start_node[1], start_node[2]] <- TRUE
  t <- start_time
  er <- end_node[1]
  ec <- end_node[2]
  while (TRUE) {
    t <- t + 1
    nxt <- curr
    nxt[2:H, ] <- nxt[2:H, ] | curr[1:(H - 1), ]
    nxt[1:(H - 1), ] <- nxt[1:(H - 1), ] | curr[2:H, ]
    nxt[, 2:W] <- nxt[, 2:W] | curr[, 1:(W - 1)]
    nxt[, 1:(W - 1)] <- nxt[, 1:(W - 1)] | curr[, 2:W]
    col_idx_R <- (ic - 2 - t) %% IW + 2
    col_idx_L <- (ic - 2 + t) %% IW + 2
    row_idx_D <- (ir - 2 - t) %% IH + 2
    row_idx_U <- (ir - 2 + t) %% IH + 2
    mask_inner <- is_R[ir, col_idx_R] | is_L[ir, col_idx_L] | is_D[row_idx_D, ic] | is_U[row_idx_U, ic]
    curr <- nxt & !walls
    curr[ir, ic] <- curr[ir, ic] & !mask_inner
    if (curr[er, ec]) return(t)
  }
}

t1 <- solve_segment(start_pos, end_pos, 0)
t2 <- solve_segment(end_pos, start_pos, t1)
t3 <- solve_segment(start_pos, end_pos, t2)
cat(t3, "\n")