generate_dragon_curve <- function(data, length) {
  while (nchar(data) < length) {
    b <- chartr("01", "10", rev(strsplit(data, NULL)[[1]]))
    data <- paste0(data, "0", paste(b, collapse = ""))
  }
  substr(data, 1, length)
}

calculate_checksum <- function(data) {
  while (nchar(data) %% 2 == 0) {
    pairs <- strsplit(data, NULL)[[1]]
    data <- sapply(seq(1, length(pairs) - 1, by = 2), function(i) {
      if (pairs[i] == pairs[i + 1]) "1" else "0"
    })
    data <- paste(data, collapse = "")
  }
  data
}

input_data <- readLines("input.txt")
initial_state <- input_data[1]
disk_length <- 272

data <- generate_dragon_curve(initial_state, disk_length)
checksum <- calculate_checksum(data)
cat(checksum, "\n")