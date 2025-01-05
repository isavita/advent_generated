
parse_input <- function(input) {
  lapply(input, function(line) {
    parts <- strsplit(line, " ")[[1]]
    springs <- parts[1]
    ints <- as.integer(strsplit(parts[2], ",")[[1]])
    list(springs = springs, group = ints)
  })
}

count_arrangements_recursive <- function(row, i_springs, i_group, i_contiguous_damaged, cache) {
  if (i_springs > nchar(row$springs)) {
    if (i_group == length(row$group) && i_contiguous_damaged == 0) {
      return(1)
    } else if (i_group == length(row$group) - 1 && i_contiguous_damaged == row$group[i_group + 1]) {
      return(1)
    }
    return(0)
  }
  
  cache_key <- paste(i_springs, i_group, i_contiguous_damaged, sep = "_")
  if (cache_key %in% names(cache)) {
    return(cache[[cache_key]])
  }
  
  res <- 0
  char <- substr(row$springs, i_springs, i_springs)
  if (char == "." || char == "?") {
    if (i_contiguous_damaged == 0) {
      res <- res + count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous_damaged, cache)
    } else if (i_group < length(row$group) && i_contiguous_damaged == row$group[i_group + 1]) {
      res <- res + count_arrangements_recursive(row, i_springs + 1, i_group + 1, 0, cache)
    }
  }
  if (char == "#" || char == "?") {
    if (i_group < length(row$group) && i_contiguous_damaged < row$group[i_group + 1]) {
      res <- res + count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous_damaged + 1, cache)
    }
  }
  
  cache[[cache_key]] <- res
  return(res)
}

count_arrangements <- function(row) {
  count_arrangements_recursive(row, 1, 0, 0, list())
}

unfold_row <- function(row, unfolding_factor) {
  new_springs <- row$springs
  new_group <- row$group
  
  if (unfolding_factor > 1) {
    for (i in 2:unfolding_factor) {
      new_springs <- paste(new_springs, "?", row$springs, sep = "")
      new_group <- c(new_group, row$group)
    }
  }
  
  list(springs = new_springs, group = new_group)
}

solve <- function(input) {
  rows <- parse_input(input)
  sum(sapply(rows, count_arrangements))
}

input <- readLines("input.txt")
result <- solve(input)
cat(result, "\n")
