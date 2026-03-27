
options(scipen = 999)
file_path <- "input.txt"
if (file.exists(file_path) && file.info(file_path)$size > 0) {
  content <- readChar(file_path, file.info(file_path)$size)
  matches <- regmatches(content, gregexpr("\\d+-\\s*\\d+", content))[[1]]
  total_sum <- 0
  for (m in matches) {
    limits <- as.numeric(strsplit(m, "-")[[1]])
    a <- min(limits)
    b <- max(limits)
    chunk_size <- 1e6
    for (start_val in seq(a, b, by = chunk_size)) {
      end_val <- min(start_val + chunk_size - 1, b)
      nums <- seq(start_val, end_val)
      is_rep <- grepl("^(.+)\\1+$", as.character(nums), perl = TRUE)
      total_sum <- total_sum + sum(nums[is_rep])
    }
  }
  cat(sprintf("%.0f\n", total_sum))
} else {
  cat("0\n")
}
