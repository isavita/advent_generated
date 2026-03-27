
options(scipen = 999)
lines <- readLines("input.txt", warn = FALSE)

# Parse seed ranges
s_raw <- as.numeric(strsplit(sub("seeds: ", "", lines[1]), "\\s+")[[1]])
s_raw <- s_raw[!is.na(s_raw)]
ranges <- matrix(s_raw, ncol = 2, byrow = TRUE)
ranges[, 2] <- ranges[, 1] + ranges[, 2] - 1

# Parse map blocks
maps <- list()
curr_m <- NULL
for (l in lines[-1]) {
  if (grepl("map:", l)) {
    if (!is.null(curr_m)) maps[[length(maps) + 1]] <- curr_m
    curr_m <- matrix(numeric(), ncol = 3, nrow = 0)
  } else {
    v <- as.numeric(strsplit(trimws(l), "\\s+")[[1]])
    if (length(v) == 3) curr_m <- rbind(curr_m, v)
  }
}
if (!is.null(curr_m)) maps[[length(maps) + 1]] <- curr_m

# Process ranges through maps
for (m in maps) {
  next_r <- matrix(numeric(), ncol = 2, nrow = 0)
  while (nrow(ranges) > 0) {
    r <- ranges[1, ]
    ranges <- ranges[-1, , drop = FALSE]
    hit <- FALSE
    for (i in seq_len(nrow(m))) {
      dest <- m[i, 1]
      src <- m[i, 2]
      len <- m[i, 3]
      src_end <- src + len - 1
      
      overlap_start <- max(r[1], src)
      overlap_end <- min(r[2], src_end)
      
      if (overlap_start <= overlap_end) {
        next_r <- rbind(next_r, c(overlap_start - src + dest, overlap_end - src + dest))
        if (overlap_start > r[1]) ranges <- rbind(ranges, c(r[1], overlap_start - 1))
        if (overlap_end < r[2]) ranges <- rbind(ranges, c(overlap_end + 1, r[2]))
        hit <- TRUE
        break
      }
    }
    if (!hit) next_r <- rbind(next_r, r)
  }
  ranges <- next_r
}

cat(format(min(ranges[, 1]), scientific = FALSE), "\n")
