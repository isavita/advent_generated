
input <- readLines("input.txt")
stones <- strsplit(trimws(input), " ")[[1]]

for (i in 1:25) {
  next_stones <- character(0)
  for (s in stones) {
    if (s == "0") {
      next_stones <- c(next_stones, "1")
    } else if (nchar(s) %% 2 == 0) {
      mid <- nchar(s) %/% 2
      left <- sub("^0+", "", substr(s, 1, mid))
      right <- sub("^0+", "", substr(s, mid + 1, nchar(s)))
      if (left == "") left <- "0"
      if (right == "") right <- "0"
      next_stones <- c(next_stones, left, right)
    } else {
      next_stones <- c(next_stones, as.character(as.integer(s) * 2024))
    }
  }
  stones <- next_stones
}

cat(length(stones), "\n")
