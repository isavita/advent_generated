lines <- readLines("input.txt", warn = FALSE)
if (length(lines) == 0) {
  cat("Grand total: 0\n")
} else {
  w <- max(nchar(lines))
  m <- do.call(rbind, strsplit(sprintf("%-*s", w, lines), ""))
  sep <- apply(m, 2, function(x) all(grepl("^\\s*$", x)))
  idx <- which(!sep)
  total <- 0
  if (length(idx)) {
    for (g in split(idx, cumsum(c(0, diff(idx) > 1)))) {
      nums <- character(0); op <- "+"
      for (c in g) {
        col_chars <- m[, c]
        d <- col_chars[grepl("[0-9]", col_chars)]
        if (length(d)) nums <- c(nums, paste0(d, collapse = ""))
        ops <- col_chars[col_chars %in% c("+", "*")]
        if (length(ops)) op <- ops[length(ops)]
      }
      if (length(nums)) {
        v <- as.numeric(nums)
        total <- total + if (op == "*") prod(v) else sum(v)
      }
    }
  }
  cat(paste0("Grand total: ", format(total, scientific = FALSE), "\n"))
}