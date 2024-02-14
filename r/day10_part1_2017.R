
data <- readLines("input.txt")
lengths <- as.integer(strsplit(data, ",")[[1]])

list <- 0:255
currentPosition <- 0
skipSize <- 0

for (length in lengths) {
  for (i in 1:(length %/% 2)) {
    start <- (currentPosition + i - 1) %% 256 + 1
    end <- (currentPosition + length - i) %% 256 + 1
    temp <- list[start]
    list[start] <- list[end]
    list[end] <- temp
  }

  currentPosition <- (currentPosition + length + skipSize) %% 256
  skipSize <- skipSize + 1
}

result <- list[1] * list[2]
cat(result, "\n")
