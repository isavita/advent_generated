particles <- readLines("input.txt")
parse_particle <- function(line) {
  parts <- strsplit(line, ", ")[[1]]
  p <- v <- a <- numeric(3)
  for (i in 1:3) {
    coords <- as.numeric(gsub(".*<|>| ", "", strsplit(parts[i], ",")[[1]]))
    if (i == 1) p <- coords
    if (i == 2) v <- coords
    if (i == 3) a <- coords
  }
  list(p = p, v = v, a = a)
}

particles <- lapply(particles, parse_particle)

for (tick in 1:1000) {
  positions <- list()
  for (i in seq_along(particles)) {
    particles[[i]]$v <- particles[[i]]$v + particles[[i]]$a
    particles[[i]]$p <- particles[[i]]$p + particles[[i]]$v
    pos_str <- paste(particles[[i]]$p, collapse = ",")
    positions[[pos_str]] <- (positions[[pos_str]] %||% 0) + 1
  }

  particles <- Filter(function(p) positions[[paste(p$p, collapse = ",")]] == 1, particles)
}

cat(length(particles), "\n")