
input <- readLines("input.txt", warn = FALSE)
input <- input[input != ""]

if (length(input) == 0) {
  cat("0\n")
} else {
  grid <- do.call(rbind, strsplit(input, ""))
  s <- which(grid == "S", arr.ind = TRUE)
  
  if (nrow(s) == 0) {
    cat("0\n")
  } else {
    res <- setNames(1, s[1, 2])
    nc <- ncol(grid)
    nr <- nrow(grid)
    
    for (y in s[1, 1]:nr) {
      k <- as.integer(names(res))
      sp <- rep(FALSE, length(k))
      v <- k >= 1 & k <= nc
      sp[v] <- grid[y, k[v]] == "^"
      
      ak <- c(k[!sp], k[sp] - 1, k[sp] + 1)
      av <- c(as.numeric(res[!sp]), as.numeric(res[sp]), as.numeric(res[sp]))
      
      if (length(ak) > 0) {
        res <- tapply(av, ak, sum)
      } else {
        res <- numeric(0)
      }
    }
    cat(sprintf("%.0f\n", sum(res)))
  }
}
