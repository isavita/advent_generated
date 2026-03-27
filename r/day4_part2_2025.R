
lines <- readLines("input.txt", warn = FALSE)
if (length(lines) == 0) {
  cat("Total rolls removed: 0\n")
} else {
  R <- length(lines)
  C <- max(nchar(lines))
  m <- matrix(".", R, C)
  for (i in 1:R) {
    s <- strsplit(lines[i], "")[[1]]
    if (length(s) > 0) m[i, 1:length(s)] <- s
  }

  total <- 0
  repeat {
    at <- (m == "@")
    if (!any(at)) break
    cnts <- matrix(0, R, C)
    for (dr in -1:1) {
      for (dc in -1:1) {
        if (dr == 0 && dc == 0) next
        r_trg <- seq(max(1, 1 + dr), min(R, R + dr))
        c_trg <- seq(max(1, 1 + dc), min(C, C + dc))
        r_src <- seq(max(1, 1 - dr), min(R, R - dr))
        c_src <- seq(max(1, 1 - dc), min(C, C - dc))
        if (length(r_trg) > 0 && length(c_trg) > 0)
          cnts[r_trg, c_trg] <- cnts[r_trg, c_trg] + at[r_src, c_src]
      }
    }
    rem <- at & (cnts < 4)
    n_rem <- sum(rem)
    if (n_rem == 0) break
    total <- total + n_rem
    m[rem] <- "."
  }
  cat(sprintf("Total rolls removed: %d\n", total))
}
