
gcd <- function(a, b) if (b == 0) a else gcd(b, a %% b)
lcm <- function(a, b) abs(a * b) / gcd(a, b)

lines <- readLines("input.txt")
grid <- do.call(rbind, strsplit(lines, ""))
rows <- nrow(grid)
cols <- ncol(grid)
H <- rows - 2
W <- cols - 2

start <- c(1, which(grid[1, ] == "."))
end <- c(rows, which(grid[rows, ] == "."))

cycle <- lcm(H, W)
visited <- array(FALSE, dim = c(rows, cols, cycle))

q <- matrix(start, ncol = 2)
t <- 0

repeat {
  t <- t + 1
  
  # Possible moves: stay, down, up, right, left
  cand <- rbind(
    q,
    cbind(q[, 1] + 1, q[, 2]),
    cbind(q[, 1] - 1, q[, 2]),
    cbind(q[, 1], q[, 2] + 1),
    cbind(q[, 1], q[, 2] - 1)
  )
  
  # Remove duplicate positions and out-of-bounds/wall cells
  cand <- cand[!duplicated(cand), , drop = FALSE]
  cand <- cand[cand[, 1] >= 1 & cand[, 1] <= rows & cand[, 2] >= 1 & cand[, 2] <= cols, , drop = FALSE]
  cand <- cand[grid[cand] != "#", , drop = FALSE]
  
  # Check for blizzards using vectorized modular arithmetic
  r <- cand[, 1]
  c <- cand[, 2]
  inner <- r > 1 & r < rows
  is_b <- rep(FALSE, nrow(cand))
  
  if (any(inner)) {
    ri <- r[inner]
    ci <- c[inner]
    # Check if a blizzard started at a position that would move to (ri, ci) at time t
    is_b[inner] <- (grid[cbind(((ri - 2 + t) %% H) + 2, ci)] == "^") |
                   (grid[cbind(((ri - 2 - t) %% H) + 2, ci)] == "v") |
                   (grid[cbind(ri, ((ci - 2 + t) %% W) + 2)] == "<") |
                   (grid[cbind(ri, ((ci - 2 - t) %% W) + 2)] == ">")
  }
  
  cand <- cand[!is_b, , drop = FALSE]
  
  # Exit condition: reached destination
  if (any(cand[, 1] == end[1] & cand[, 2] == end[2])) {
    cat(t, "\n")
    break
  }
  
  # Filter visited states (position + time cycle) to prune search space
  if (nrow(cand) > 0) {
    idx <- (t %% cycle) + 1
    new_states <- !visited[cbind(cand[, 1], cand[, 2], idx)]
    q <- cand[new_states, , drop = FALSE]
    if (nrow(q) > 0) {
      visited[cbind(q[, 1], q[, 2], idx)] <- TRUE
    }
  } else {
    q <- cand
  }
  
  if (t > 1e5) break # Safety break
}
