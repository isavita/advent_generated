solve <- function(b, time) {
  max_g <- 0
  m_ore <- max(b[c(1, 2, 3, 5)])
  
  dfs <- function(t, r1, r2, r3, r4, m1, m2, m3, g) {
    if (g + r4 * t + (t * (t - 1)) %/% 2 <= max_g) return()
    if (g + r4 * t > max_g) max_g <<- g + r4 * t
    if (t <= 1) return()
    
    # Geode Robot
    w <- max(0, ceiling((b[6] - m3) / r3), ceiling((b[5] - m1) / r1))
    if (!is.infinite(w) && w < t - 1) {
      d <- w + 1
      dfs(t - d, r1, r2, r3, r4 + 1, m1 + r1 * d - b[5], m2 + r2 * d, m3 + r3 * d - b[6], g + r4 * d)
      if (w == 0) return()
    }
    
    # Obsidian Robot
    w <- max(0, ceiling((b[4] - m2) / r2), ceiling((b[3] - m1) / r1))
    if (r3 < b[6] && !is.infinite(w) && w < t - 1) {
      d <- w + 1
      dfs(t - d, r1, r2, r3 + 1, r4, m1 + r1 * d - b[3], m2 + r2 * d - b[4], m3 + r3 * d, g + r4 * d)
    }
    
    # Clay Robot
    w <- max(0, ceiling((b[2] - m1) / r1))
    if (r2 < b[4] && !is.infinite(w) && w < t - 1) {
      d <- w + 1
      dfs(t - d, r1, r2 + 1, r3, r4, m1 + r1 * d - b[2], m2 + r2 * d, m3 + r3 * d, g + r4 * d)
    }
    
    # Ore Robot
    w <- max(0, ceiling((b[1] - m1) / r1))
    if (r1 < m_ore && !is.infinite(w) && w < t - 1) {
      d <- w + 1
      dfs(t - d, r1 + 1, r2, r3, r4, m1 + r1 * d - b[1], m2 + r2 * d, m3 + r3 * d, g + r4 * d)
    }
  }
  
  dfs(time, 1, 0, 0, 0, 0, 0, 0, 0)
  return(max_g)
}

lines <- readLines("input.txt")
total_prod <- 1
n <- min(length(lines), 3)

for (i in 1:n) {
  nums <- as.numeric(unlist(regmatches(lines[i], gregexpr("\\d+", lines[i]))))
  res <- solve(nums[2:7], 32)
  total_prod <- total_prod * res
}

cat(format(total_prod, scientific = FALSE), "\n")