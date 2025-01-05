
hex_to_bin <- function(hex) {
  sapply(strsplit(hex, "")[[1]], function(h) {
    paste(rev(as.integer(intToBits(strtoi(h, 16L)))[1:4]), collapse = "")
  }) |> paste(collapse = "")
}

parse_packet <- function(bin_str, idx) {
  version <- sum(as.integer(strsplit(substr(bin_str, idx + 1, idx + 3), "")[[1]]) * c(4, 2, 1))
  type_id <- sum(as.integer(strsplit(substr(bin_str, idx + 4, idx + 6), "")[[1]]) * c(4, 2, 1))
  idx <- idx + 6

  if (type_id == 4) {
    while (substr(bin_str, idx + 1, idx + 1) == "1") {
      idx <- idx + 5
    }
    idx <- idx + 5
    return(list(version, idx))
  }

  length_type_id <- as.integer(substr(bin_str, idx + 1, idx + 1))
  idx <- idx + 1
  num_sub_packets <- 0
  sub_packet_length <- 0

  if (length_type_id == 0) {
    sub_packet_length <- sum(as.integer(strsplit(substr(bin_str, idx + 1, idx + 15), "")[[1]]) * 2^(14:0))
    idx <- idx + 15
  } else {
    num_sub_packets <- sum(as.integer(strsplit(substr(bin_str, idx + 1, idx + 11), "")[[1]]) * 2^(10:0))
    idx <- idx + 11
  }

  version_sum <- version
  while (TRUE) {
    if (length_type_id == 0 && sub_packet_length == 0) {
      break
    }
    if (length_type_id == 1 && num_sub_packets == 0) {
      break
    }
    result <- parse_packet(bin_str, idx)
    sub_version <- result[[1]]
    new_index <- result[[2]]
    version_sum <- version_sum + sub_version

    if (length_type_id == 0) {
      sub_packet_length <- sub_packet_length - (new_index - idx)
    } else {
      num_sub_packets <- num_sub_packets - 1
    }
    idx <- new_index
  }
  return(list(version_sum, idx))
}

data <- readLines("input.txt", warn = FALSE)
hex_str <- trimws(data)
bin_str <- hex_to_bin(hex_str)
result <- parse_packet(bin_str, 0)
cat(result[[1]], "\n")
