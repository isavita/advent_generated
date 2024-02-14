data <- scan("input.txt")
total_liters <- 150

count_combinations <- function(data, total_liters) {
  if (total_liters == 0) {
    return(1)
  }
  if (length(data) == 0 | total_liters < 0) {
    return(0)
  }
  
  return(count_combinations(data[-1], total_liters) + count_combinations(data[-1], total_liters - data[1]))
}

result <- count_combinations(data, total_liters)
print(result)