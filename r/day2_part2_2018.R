
lines <- readLines("input.txt")
for (i in 1:(length(lines)-1)) {
  for (j in (i+1):length(lines)) {
    diff <- 0
    for (k in 1:nchar(lines[[i]])) {
      if (substr(lines[[i]], k, k) != substr(lines[[j]], k, k)) {
        diff <- diff + 1
        if (diff > 1) {
          break
        }
      }
    }
    if (diff == 1) {
      common <- ""
      for (k in 1:nchar(lines[[i]])) {
        if (substr(lines[[i]], k, k) == substr(lines[[j]], k, k)) {
          common <- paste0(common, substr(lines[[i]], k, k))
        }
      }
      cat(common, "\n")
      quit("no")
    }
  }
}
