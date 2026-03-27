
options(scipen = 999)
raw <- readChar("input.txt", file.info("input.txt")$size)
parts <- strsplit(gsub("[ \r\n]", "", raw), ",")[[1]]
ranges <- do.call(rbind, lapply(strsplit(parts, "-"), as.numeric))

max_val <- max(ranges)
max_k <- ceiling(nchar(format(max_val, scientific = FALSE)) / 2)
invalid_ids <- numeric(0)

for (k in 1:max_k) {
  mult <- 10^k + 1
  min_seed <- if (k == 1) 1 else 10^(k - 1)
  max_seed <- 10^k - 1
  
  for (i in 1:nrow(ranges)) {
    s_min <- ceiling(ranges[i, 1] / mult)
    s_max <- floor(ranges[i, 2] / mult)
    
    low <- max(s_min, min_seed)
    high <- min(s_max, max_seed)
    
    if (low <= high) {
      invalid_ids <- c(invalid_ids, (low:high) * mult)
    }
  }
}

total <- sum(unique(invalid_ids))
cat(sprintf("Sum of invalid IDs: %.0f\n", total))
