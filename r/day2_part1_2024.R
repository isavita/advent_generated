
parse_levels <- function(line) {
  as.integer(strsplit(line, " ")[[1]])
}

is_safe_report <- function(levels) {
  if (length(levels) < 2) return(FALSE)
  first_diff <- levels[2] - levels[1]
  if (first_diff == 0) return(FALSE)
  is_increasing <- first_diff > 0
  
  diffs <- diff(levels)
  if(any(diffs == 0)) return(FALSE)
  
  if(is_increasing){
    if(any(diffs <= 0)) return(FALSE)
  } else {
    if(any(diffs >= 0)) return(FALSE)
  }
  
  abs_diffs <- abs(diffs)
  if(any(abs_diffs < 1 | abs_diffs > 3)) return(FALSE)
  
  TRUE
}

lines <- readLines("input.txt")
levels_list <- lapply(lines, parse_levels)
safe_report_count <- sum(sapply(levels_list, is_safe_report))
cat(safe_report_count, "\n")
