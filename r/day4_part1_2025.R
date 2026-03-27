
lines <- readLines("input.txt", warn = FALSE)
lines <- lines[nchar(lines) > 0]

if (length(lines) == 0) {
  cat(0, "\n")
} else {
  grid <- do.call(rbind, strsplit(lines, ""))
  pts <- which(grid == "@", arr.ind = TRUE)
  
  if (nrow(pts) == 0) {
    cat(0, "\n")
  } else {
    nr <- nrow(grid)
    nc <- ncol(grid)
    ans <- sum(apply(pts, 1, function(x) {
      r <- x[1]
      c <- x[2]
      sub <- grid[max(1, r - 1):min(nr, r + 1), max(1, c - 1):min(nc, c + 1)]
      (sum(sub == "@") - 1) < 4
    }))
    cat(ans, "\n")
  }
}
