
options(scipen = 999)

input <- readLines("input.txt")
input <- input[input != ""]
grid <- do.call(rbind, strsplit(input, ""))
N <- nrow(grid)
start_pos <- which(grid == "S", arr.ind = TRUE)
grid[start_pos] <- "."

target <- 26501365
rem <- target %% N
vals <- numeric(3)
curr_pos <- start_pos

dirs <- matrix(c(0, 1, 0, -1, 1, 0, -1, 0), ncol = 2, byrow = TRUE)

for (i in 1:(rem + 2 * N)) {
  nr <- nrow(curr_pos)
  next_pos <- matrix(0, nrow = nr * 4, ncol = 2)
  next_pos[, 1] <- rep(curr_pos[, 1], each = 4) + dirs[, 1]
  next_pos[, 2] <- rep(curr_pos[, 2], each = 4) + dirs[, 2]
  
  m_idx_r <- ((next_pos[, 1] - 1) %% N) + 1
  m_idx_c <- ((next_pos[, 2] - 1) %% N) + 1
  
  keep <- grid[cbind(m_idx_r, m_idx_c)] != "#"
  next_pos <- next_pos[keep, , drop = FALSE]
  
  h <- next_pos[, 1] * 1000000 + next_pos[, 2]
  curr_pos <- next_pos[!duplicated(h), , drop = FALSE]
  
  if (i == rem) {
    vals[1] <- nrow(curr_pos)
  } else if (i == rem + N) {
    vals[2] <- nrow(curr_pos)
  } else if (i == rem + 2 * N) {
    vals[3] <- nrow(curr_pos)
    break
  }
}

n <- target %/% N
y0 <- vals[1]
y1 <- vals[2]
y2 <- vals[3]

ans <- y0 + n * (y1 - y0) + (n * (n - 1) / 2) * (y2 - 2 * y1 + y0)
cat(sprintf("%.0f\n", ans))
