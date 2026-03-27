
options(scipen = 999)
if (file.exists("input.txt")) {
  initials <- scan("input.txt", what = numeric(), quiet = TRUE)
  if (length(initials) == 0) {
    cat("0\n")
  } else {
    n <- length(initials)
    prices <- matrix(0, nrow = n, ncol = 2001)
    curr <- initials
    prices[, 1] <- curr %% 10
    for (j in 2:2001) {
      curr <- bitwXor(curr, (curr * 64) %% 16777216) %% 16777216
      curr <- bitwXor(curr, bitwShiftR(curr, 5)) %% 16777216
      curr <- bitwXor(curr, (curr * 2048) %% 16777216) %% 16777216
      prices[, j] <- curr %% 10
    }
    
    global_sum <- numeric(130321)
    p1 <- 19
    p2 <- 361
    p3 <- 6859
    
    for (i in 1:n) {
      p_i <- prices[i, ]
      d9 <- p_i[2:2001] - p_i[1:2000] + 9
      idx <- d9[1:1997] + d9[2:1998] * p1 + d9[3:1999] * p2 + d9[4:2000] * p3 + 1
      
      seen <- !duplicated(idx)
      u_idx <- idx[seen]
      u_prices <- p_i[5:2001][seen]
      
      global_sum[u_idx] <- global_sum[u_idx] + u_prices
    }
    cat(max(global_sum), "\n")
  }
}
