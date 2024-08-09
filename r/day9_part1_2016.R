decompress_length <- function(input) {
  total_length <- 0
  i <- 1
  n <- nchar(input)
  
  while (i <= n) {
    if (substr(input, i, i) == "(") {
      marker_end <- regexpr("\\)", substr(input, i, n)) + i - 1
      marker <- substr(input, i + 1, marker_end - 1)
      dims <- as.integer(unlist(strsplit(marker, "x")))
      length_to_repeat <- dims[1]
      repeat_count <- dims[2]
      
      total_length <- total_length + repeat_count * length_to_repeat
      i <- marker_end + 1 + length_to_repeat
    } else {
      total_length <- total_length + 1
      i <- i + 1
    }
  }
  
  return(total_length)
}

main <- function() {
  input <- gsub("\\s+", "", readLines("input.txt"))
  result <- decompress_length(input)
  cat(result, "\n")
}

main()