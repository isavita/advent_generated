read_input <- function(filename) {
  as.integer(unlist(strsplit(trimws(readLines(filename)), " ")))
}

parse_tree <- function(data, index) {
  child_count <- data[index]
  meta_count <- data[index + 1]
  index <- index + 2
  
  total <- 0
  for (i in seq_len(child_count)) {
    result <- parse_tree(data, index)
    total <- total + result$total
    index <- result$index
  }
  
  total <- total + sum(data[index:(index + meta_count - 1)])
  index <- index + meta_count
  
  list(total = total, index = index)
}

numbers <- read_input("input.txt")
result <- parse_tree(numbers, 1)
print(result$total)