
options(scipen = 999)

calc <- function(s) {
  t <- strsplit(trimws(s), "\\s+")[[1]]
  v <- as.numeric(t[1])
  if (length(t) > 1) {
    for (i in seq(2, length(t), by = 2)) {
      if (t[i] == "+") v <- v + as.numeric(t[i + 1])
      else v <- v * as.numeric(t[i + 1])
    }
  }
  v
}

solve_line <- function(s) {
  while (grepl("\\(", s)) {
    m <- regexpr("\\([^()]+\\)", s)
    L <- attr(m, "match.length")
    inner <- substr(s, m + 1, m + L - 2)
    s <- paste0(substr(s, 1, m - 1), calc(inner), substr(s, m + L, nchar(s)))
  }
  calc(s)
}

lines <- readLines("input.txt", warn = FALSE)
ans <- sum(vapply(lines[lines != ""], solve_line, 0))
cat(format(ans, scientific = FALSE), "\n")
