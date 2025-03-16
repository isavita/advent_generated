
knot_hash <- function(input) {
  lengths <- c(utf8ToInt(input), 17, 31, 73, 47, 23)
  nums <- 0:255
  pos <- 0
  skip <- 0
  
  for (. in 1:64) {
    for (length in lengths) {
      for (i in 0:floor(length / 2 - 1)) {
          a <- (pos + i) %% 256 + 1
          b <- (pos + length - 1 - i) %% 256 + 1
          tmp <- nums[a]
          nums[a] <- nums[b]
          nums[b] <- tmp
      }
      pos <- pos + length + skip
      skip <- skip + 1
    }
  }
  
  dense_hash <- numeric(16)
    for (i in 1:16) {
    xored <- nums[((i - 1) * 16 + 1)]
    for (j in 2:16) {
      xored <- bitwXor(xored, nums[((i - 1) * 16 + j)])
    }
    dense_hash[i] <- xored
  }

  paste0(sapply(dense_hash, function(num) {
    paste(as.integer(intToBits(num))[1:8], collapse = "")
    }), collapse = "")
}

main <- function() {
  key <- readLines("input.txt", warn = FALSE)
  grid <- sapply(0:127, function(i) knot_hash(paste0(key, "-", i)))
  print(sum(sapply(grid, function(row) sum(as.integer(strsplit(row, "")[[1]])))))
}

main()
