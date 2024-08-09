captcha_sum <- function(input) {
  digits <- as.integer(unlist(strsplit(input, "")))
  n <- length(digits)
  sum(digits[digits == c(digits[-1], digits[1])])
}

input_data <- readLines("input.txt")
result <- captcha_sum(input_data)
cat(result, "\n")