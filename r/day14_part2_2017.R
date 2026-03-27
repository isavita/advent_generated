
input_str <- trimws(readLines("input.txt", warn = FALSE))

knot_hash_bits <- function(key) {
  lengths <- c(utf8ToInt(key), 17, 31, 73, 47, 23)
  lst <- 0:255
  pos <- 0
  skip <- 0
  for (round in 1:64) {
    for (len in lengths) {
      if (len > 1) {
        idx <- (pos + 0:(len - 1)) %% 256
        lst[idx + 1] <- rev(lst[idx + 1])
      }
      pos <- (pos + len + skip) %% 256
      skip <- skip + 1
    }
  }
  dense <- sapply(0:15, function(i) {
    res <- lst[i * 16 + 1]
    for (j in 1:15) res <- bitwXor(res, lst[i * 16 + j + 1])
    res
  })
  as.integer(unlist(lapply(dense, function(x) as.integer(intToBits(x)[8:1]))))
}

grid <- matrix(0, 128, 128)
for (i in 0:127) {
  grid[i + 1, ] <- knot_hash_bits(paste0(input_str, "-", i))
}

visited <- matrix(FALSE, 128, 128)
regions <- 0
for (r in 1:128) {
  for (c in 1:128) {
    if (grid[r, c] == 1 && !visited[r, c]) {
      regions <- regions + 1
      stack <- list(c(r, c))
      visited[r, c] <- TRUE
      while (length(stack) > 0) {
        curr <- stack[[length(stack)]]
        stack[[length(stack)]] <- NULL
        cr <- curr[1]
        cc <- curr[2]
        for (d in list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))) {
          nr <- cr + d[1]
          nc <- cc + d[2]
          if (nr >= 1 && nr <= 128 && nc >= 1 && nc <= 128 && grid[nr, nc] == 1 && !visited[nr, nc]) {
            visited[nr, nc] <- TRUE
            stack[[length(stack) + 1]] <- c(nr, nc)
          }
        }
      }
    }
  }
}

cat(regions, "\n")
