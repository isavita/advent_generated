stars <- readLines("input.txt")
pattern <- "position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>"
star_data <- lapply(stars, function(line) {
  match <- regmatches(line, regexec(pattern, line))
  if (length(match[[1]]) > 1) {
    return(as.numeric(match[[1]][-1]))
  }
  NULL
})
star_data <- do.call(rbind, star_data)
colnames(star_data) <- c("x", "y", "vX", "vY")

min_area <- Inf
best_time <- 0
for (t in 1:100000) {
  coords <- star_data[, 1:2] + star_data[, 3:4] * t
  min_x <- min(coords[, 1])
  max_x <- max(coords[, 1])
  min_y <- min(coords[, 2])
  max_y <- max(coords[, 2])
  area <- (max_x - min_x + 1) * (max_y - min_y + 1)
  if (area < min_area) {
    min_area <- area
    best_time <- t
  }
}

final_coords <- star_data[, 1:2] + star_data[, 3:4] * best_time
min_x <- min(final_coords[, 1])
max_x <- max(final_coords[, 1])
min_y <- min(final_coords[, 2])
max_y <- max(final_coords[, 2])

output <- matrix(" ", nrow = max_y - min_y + 1, ncol = max_x - min_x + 1)
for (i in seq_len(nrow(final_coords))) {
  x <- final_coords[i, 1] - min_x + 1
  y <- final_coords[i, 2] - min_y + 1
  output[y, x] <- "#"
}

cat(apply(output, 1, paste0, collapse = ""), sep = "\n")