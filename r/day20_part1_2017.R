closest_particle <- function(file) {
  particles <- readLines(file)
  particles <- gsub("p=<|>,|v=<|>,|a=<|>", "", particles)
  particles <- strsplit(particles, " ")
  
  positions <- lapply(particles, function(x) {
    p <- as.numeric(unlist(strsplit(x[1], ",")))
    v <- as.numeric(unlist(strsplit(x[2], ",")))
    a <- as.numeric(unlist(strsplit(x[3], ",")))
    list(p = p, v = v, a = a)
  })

  for (i in 1:1000) {
    for (j in seq_along(positions)) {
      pos <- positions[[j]]
      pos$v <- pos$v + pos$a
      pos$p <- pos$p + pos$v
      positions[[j]] <- pos
    }
  }

  distances <- sapply(positions, function(p) sum(abs(p$p)))
  which.min(distances) - 1
}

cat("Closest particle index:", closest_particle("input.txt"), "\n")