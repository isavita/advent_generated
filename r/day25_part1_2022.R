from_snafu <- function(s) {
  n <- 0
  for (i in seq_along(s)) {
    n <- n * 5
    if (s[i] == '=') n <- n - 2
    else if (s[i] == '-') n <- n - 1
    else n <- n + as.integer(s[i])
  }
  n
}

to_snafu <- function(n) {
  b <- character()
  while (n > 0) {
    r <- n %% 5
    if (r == 3) {
      n <- n + 5
      b <- c(b, '=')
    } else if (r == 4) {
      n <- n + 5
      b <- c(b, '-')
    } else {
      b <- c(b, as.character(r))
    }
    n <- n %/% 5
  }
  paste(rev(b), collapse = "")
}

sum <- 0
lines <- readLines("input.txt")
for (line in lines) {
  sum <- sum + from_snafu(strsplit(line, "")[[1]])
}
cat(to_snafu(sum), "\n")