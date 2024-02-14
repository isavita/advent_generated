
data <- scan("input.txt", what = character(), sep = "-")

data <- as.numeric(data)

count <- 0
for (i in data[1]:data[2]) {
  digits <- as.numeric(strsplit(as.character(i), "")[[1]])
  if (any(diff(digits) == 0) && all(diff(digits) >= 0)) {
    count <- count + 1
  }
}

print(count)
