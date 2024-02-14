
input <- scan("input.txt")
preamble_length <- 25

find_invalid_number <- function(input, preamble_length) {
  for (i in (preamble_length + 1):length(input)) {
    valid <- FALSE
    for (j in (i - preamble_length):(i - 2)) {
      for (k in (j + 1):(i - 1)) {
        if (input[i] == input[j] + input[k]) {
          valid <- TRUE
          break
        }
      }
      if (valid) {
        break
      }
    }
    if (!valid) {
      return(input[i])
    }
  }
}

result <- find_invalid_number(input, preamble_length)
print(result)
