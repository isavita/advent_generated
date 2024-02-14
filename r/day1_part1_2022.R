
lines <- readLines("input.txt")
maxCalories <- 0
currentCalories <- 0

for (line in lines) {
  if (line == "") {
    if (currentCalories > maxCalories) {
      maxCalories <- currentCalories
    }
    currentCalories <- 0
    next
  }
  
  calories <- as.integer(line)
  if (is.na(calories)) {
    stop("Error converting line to int")
  }
  
  currentCalories <- currentCalories + calories
}

if (currentCalories > maxCalories) {
  maxCalories <- currentCalories
}

cat(maxCalories, "\n")
