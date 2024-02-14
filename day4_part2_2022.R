
data <- readLines("input.txt")
count <- 0

for (pair in data) {
  ranges <- strsplit(pair, ",")[[1]]
  
  left <- as.numeric(unlist(strsplit(ranges[1], "-")))
  right <- as.numeric(unlist(strsplit(ranges[2], "-")))
  
  if (left[1] <= right[2] && left[2] >= right[1]) {
    count <- count + 1
  }
}

cat(count)
