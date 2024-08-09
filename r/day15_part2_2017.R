dueling_generators <- function() {
  input <- readLines("input.txt")
  start_a <- as.numeric(sub(".*: (\\d+)", "\\1", input[1]))
  start_b <- as.numeric(sub(".*: (\\d+)", "\\1", input[2]))
  
  count_part_one <- function(a, b, pairs) {
    count <- 0
    for (i in 1:pairs) {
      a <- (a * 16807) %% 2147483647
      b <- (b * 48271) %% 2147483647
      if ((bitwAnd(a, 0xFFFF) == bitwAnd(b, 0xFFFF))) {
        count <- count + 1
      }
    }
    count
  }
  
  count_part_two <- function(a, b, pairs) {
    count <- 0
    for (i in 1:pairs) {
      repeat {
        a <- (a * 16807) %% 2147483647
        if (a %% 4 == 0) break
      }
      repeat {
        b <- (b * 48271) %% 2147483647
        if (b %% 8 == 0) break
      }
      if ((bitwAnd(a, 0xFFFF) == bitwAnd(b, 0xFFFF))) {
        count <- count + 1
      }
    }
    count
  }
  
  part_one_result <- count_part_one(start_a, start_b, 40000000)
  part_two_result <- count_part_two(start_a, start_b, 5000000)
  
  cat("Part One Count:", part_one_result, "\n")
  cat("Part Two Count:", part_two_result, "\n")
}

dueling_generators()