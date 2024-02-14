
input <- scan("input.txt")
invalid_number <- 14360655

for (i in 1:(length(input)-1)) {
  for (j in (i+1):length(input)) {
    if (sum(input[i:j]) == invalid_number) {
      weakness <- min(input[i:j]) + max(input[i:j])
      print(weakness)
    }
  }
}
