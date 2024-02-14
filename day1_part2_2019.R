input <- as.numeric(readLines("input.txt"))

calculate_fuel <- function(mass) {
  fuel <- max(0, floor(mass / 3) - 2)
  if (fuel > 0) {
    return(fuel + calculate_fuel(fuel))
  } else {
    return(fuel)
  }
}

total_fuel <- sum(sapply(input, calculate_fuel))
print(total_fuel)