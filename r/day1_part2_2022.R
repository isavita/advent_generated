
input <- readLines("input.txt")
caloriesList <- c()
currentCalories <- 0

for (line in input) {
  if (line == "") {
    caloriesList <- c(caloriesList, currentCalories)
    currentCalories <- 0
    next
  }

  calories <- as.integer(line)
  if (is.na(calories)) {
    stop("Error converting line to int")
  }

  currentCalories <- currentCalories + calories
}

caloriesList <- c(caloriesList, currentCalories)
caloriesList <- sort(caloriesList, decreasing = TRUE)

topThreeSum <- 0
for (i in 1:3) {
  if (i > length(caloriesList)) {
    break
  }
  topThreeSum <- topThreeSum + caloriesList[i]
}

cat(topThreeSum, "\n")
