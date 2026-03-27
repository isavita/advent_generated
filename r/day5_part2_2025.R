
if (file.exists("input.txt")) {
  lines <- readLines("input.txt", warn = FALSE)
  lines <- lines[grep("-", lines)]
  
  if (length(lines) == 0) {
    cat("Total fresh IDs: 0\n")
  } else {
    ranges <- do.call(rbind, lapply(strsplit(lines, "-"), function(x) sort(as.numeric(trimws(x)))))
    ranges <- ranges[order(ranges[, 1]), , drop = FALSE]
    
    total <- 0
    cur_min <- ranges[1, 1]
    cur_max <- ranges[1, 2]
    
    if (nrow(ranges) > 1) {
      for (i in 2:nrow(ranges)) {
        if (ranges[i, 1] <= cur_max) {
          cur_max <- max(cur_max, ranges[i, 2])
        } else {
          total <- total + (cur_max - cur_min + 1)
          cur_min <- ranges[i, 1]
          cur_max <- ranges[i, 2]
        }
      }
    }
    total <- total + (cur_max - cur_min + 1)
    cat(sprintf("Total fresh IDs: %.0f\n", total))
  }
}
