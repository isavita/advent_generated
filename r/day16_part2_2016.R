generate_dragon_curve <- function(initial, length) {
  while (nchar(initial) < length) {
    b <- gsub("0", "2", rev(strsplit(initial, NULL)[[1]]))  # Reverse and swap 0s and 1s
    b <- gsub("1", "0", b)
    b <- gsub("2", "1", b)
    initial <- paste0(initial, "0", paste(b, collapse = ""))
  }
  return(substr(initial, 1, length))
}

calculate_checksum <- function(data) {
  while (nchar(data) %% 2 == 0) {
    pairs <- strsplit(data, NULL)[[1]]
    data <- sapply(seq(1, length(pairs), by = 2), function(i) {
      if (pairs[i] == pairs[i + 1]) "1" else "0"
    })
    data <- paste(data, collapse = "")
  }
  return(data)
}

input <- readLines("input.txt")
initial_state <- input[1]

# Part One
disk_length_1 <- 272
data_1 <- generate_dragon_curve(initial_state, disk_length_1)
checksum_1 <- calculate_checksum(data_1)
cat("Checksum for disk length 272:", checksum_1, "\n")

# Part Two
disk_length_2 <- 35651584
data_2 <- generate_dragon_curve(initial_state, disk_length_2)
checksum_2 <- calculate_checksum(data_2)
cat("Checksum for disk length 35651584:", checksum_2, "\n")