
# Read input from file
input <- as.numeric(readLines("input.txt"))

# Calculate fuel requirements
fuel_requirements <- floor(input / 3) - 2

# Calculate total fuel requirement
total_fuel <- sum(fuel_requirements)

print(total_fuel)
