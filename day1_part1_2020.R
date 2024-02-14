data <- scan("input.txt")
result <- 0
for (i in 1:length(data)) {
  for (j in (i+1):length(data)) {
    if (data[i] + data[j] == 2020) {
      result <- data[i] * data[j]
      break
    }
  }
  if (result != 0) {
    break
  }
}
print(result)