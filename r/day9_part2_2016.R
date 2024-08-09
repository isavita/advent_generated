decompress_length_v1 <- function(data) {
  len <- 0
  i <- 1
  while (i <= nchar(data)) {
    if (substring(data, i, i) == "(") {
      marker_end <- regexpr("\\)", substring(data, i)) + i - 1
      marker <- substring(data, i + 1, marker_end - 1)
      parts <- as.integer(unlist(strsplit(marker, "x")))
      chars_to_repeat <- substring(data, marker_end + 1, marker_end + parts[1])
      len <- len + nchar(chars_to_repeat) * parts[2]
      i <- marker_end + parts[1] + 1
    } else {
      len <- len + 1
      i <- i + 1
    }
  }
  return(len)
}

decompress_length_v2 <- function(data) {
  calculate_length <- function(start, end) {
    total_length <- 0
    i <- start
    while (i <= end) {
      if (substring(data, i, i) == "(") {
        marker_end <- regexpr("\\)", substring(data, i)) + i - 1
        marker <- substring(data, i + 1, marker_end - 1)
        parts <- as.integer(unlist(strsplit(marker, "x")))
        chars_to_repeat <- parts[1]
        repeat_count <- parts[2]
        total_length <- total_length + repeat_count * calculate_length(marker_end + 1, marker_end + chars_to_repeat)
        i <- marker_end + chars_to_repeat + 1
      } else {
        total_length <- total_length + 1
        i <- i + 1
      }
    }
    return(total_length)
  }
  return(calculate_length(1, nchar(data)))
}

main <- function() {
  input_data <- gsub("\\s", "", readLines("input.txt"))
  part1_result <- decompress_length_v1(input_data)
  part2_result <- decompress_length_v2(input_data)
  
  cat("Part 1 Decompressed Length:", part1_result, "\n")
  cat("Part 2 Decompressed Length:", part2_result, "\n")
}

main()