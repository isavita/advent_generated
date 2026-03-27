options(scipen = 999)
line <- readLines("input.txt", warn = FALSE)
if (length(line) == 0) {
  cat("0\n")
} else {
  d <- as.integer(unlist(strsplit(gsub("[^0-9]", "", line[1]), "")))
  n <- length(d)
  if (n == 0) {
    cat("0\n")
  } else {
    disk <- rep(-1, sum(d))
    num_files <- (n + 1) %/% 2
    f_starts <- numeric(num_files); f_lens <- numeric(num_files)
    free_starts <- numeric(n %/% 2); free_lens <- numeric(n %/% 2)
    curr <- 0; fid <- 0; fidx <- 1
    for (i in 1:n) {
      l <- d[i]
      if (i %% 2 != 0) {
        if (l > 0) disk[(curr + 1):(curr + l)] <- fid
        f_starts[fid + 1] <- curr; f_lens[fid + 1] <- l; fid <- fid + 1
      } else {
        if (l > 0) {
          free_starts[fidx] <- curr; free_lens[fidx] <- l; fidx <- fidx + 1
        }
      }
      curr <- curr + l
    }
    free_count <- fidx - 1
    for (i in rev(seq_len(num_files))) {
      flen <- f_lens[i]; fstart <- f_starts[i]
      if (flen == 0) next
      for (j in seq_len(free_count)) {
        if (free_starts[j] >= fstart) break
        if (free_lens[j] >= flen) {
          disk[(free_starts[j] + 1):(free_starts[j] + flen)] <- i - 1
          disk[(fstart + 1):(fstart + flen)] <- -1
          free_starts[j] <- free_starts[j] + flen
          free_lens[j] <- free_lens[j] - flen
          break
        }
      }
    }
    v <- which(disk != -1)
    cat(format(sum(as.numeric(v - 1) * disk[v]), scientific = FALSE), "\n")
  }
}