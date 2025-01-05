
is_safe_report <- function(levels) {
  n <- length(levels)
  if (n < 2) return(FALSE)
  first_diff <- levels[2] - levels[1]
  if (first_diff == 0) return(FALSE)
  is_increasing <- first_diff > 0
  for (i in 1:(n - 1)) {
    diff <- levels[i + 1] - levels[i]
    if (diff == 0) return(FALSE)
    if ((is_increasing && diff <= 0) || (!is_increasing && diff >= 0)) return(FALSE)
    abs_diff <- abs(diff)
    if (abs_diff < 1 || abs_diff > 3) return(FALSE)
  }
  TRUE
}

is_safe_with_one_removal <- function(levels) {
  n <- length(levels)
  if(n < 2) return(FALSE)
  for (i in 1:n) {
    modified_levels <- levels[-i]
    if (is_safe_report(modified_levels)) return(TRUE)
  }
  FALSE
}

process_line <- function(line) {
  levels <- as.integer(strsplit(line, " ")[[1]])
  is_safe_report(levels) || is_safe_with_one_removal(levels)
}

lines <- readLines("input.txt")
safe_count <- sum(sapply(lines, process_line))
cat(safe_count, "\n")
