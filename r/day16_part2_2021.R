
hex_to_bin <- function(hex) {
  sapply(strsplit(hex, "")[[1]], function(h) {
    paste(rev(as.integer(intToBits(strtoi(h, 16L))))[29:32], collapse = "")
  }, USE.NAMES = FALSE) |> paste(collapse = "")
}

parse_packet <- function(bin_str, idx) {
  version <- strtoi(substr(bin_str, idx, idx + 2), 2L)
  type_id <- strtoi(substr(bin_str, idx + 3, idx + 5), 2L)
  idx <- idx + 6
  
  if (type_id == 4) {
    value <- 0
    repeat {
      group <- substr(bin_str, idx, idx + 4)
      value <- (value * 16) + strtoi(substr(group, 2, 5), 2L)
      idx <- idx + 5
      if (substr(group, 1, 1) == '0') break
    }
    return(list(version, idx, value))
  }
  
  length_type_id <- strtoi(substr(bin_str, idx, idx), 2L)
  idx <- idx + 1
  
  if (length_type_id == 0) {
    sub_packet_length <- strtoi(substr(bin_str, idx, idx + 14), 2L)
    idx <- idx + 15
    target_idx <- idx + sub_packet_length
    values <- numeric()
    while (idx < target_idx) {
      result <- parse_packet(bin_str, idx)
      values <- c(values, result[[3]])
      idx <- result[[2]]
    }
  } else {
    num_sub_packets <- strtoi(substr(bin_str, idx, idx + 10), 2L)
    idx <- idx + 11
    values <- numeric()
    for (i in 1:num_sub_packets) {
      result <- parse_packet(bin_str, idx)
      values <- c(values, result[[3]])
      idx <- result[[2]]
    }
  }
  
  result <- switch(type_id + 1,
                   sum(values),
                   prod(values),
                   min(values),
                   max(values),
                   NA,
                   as.integer(values[1] > values[2]),
                   as.integer(values[1] < values[2]),
                   as.integer(values[1] == values[2])
  )
  
  return(list(version, idx, result))
}

main <- function() {
  hex_str <- readLines("input.txt", warn = FALSE)
  bin_str <- hex_to_bin(hex_str)
  result <- parse_packet(bin_str, 1)
  cat(result[[3]], "\n")
}

main()
