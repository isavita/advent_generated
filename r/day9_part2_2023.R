
parseInput <- function(input) {
  histories <- list()
  for (line in input) {
    numbers <- as.integer(unlist(strsplit(line, " ")))
    histories <- c(histories, list(numbers))
  }
  return(histories)
}

allZeros <- function(nums) {
  for (num in nums) {
    if (num != 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

calculateExtrapolation <- function(history) {
  extrapolations <- c()
  for (i in 2:length(history)) {
    extrapolation <- history[i] - history[i-1]
    extrapolations <- c(extrapolations, extrapolation)
  }
  return(extrapolations)
}

calculateExtrapolations <- function(history) {
  extrapolationsSeries <- list()
  extrapolationsSeries <- c(extrapolationsSeries, list(history))

  for (i in 2:length(history)) {
    previousExtrapolations <- unlist(extrapolationsSeries[i-1])
    if (allZeros(previousExtrapolations)) {
      return(extrapolationsSeries)
    }

    extrapolations <- calculateExtrapolation(previousExtrapolations)
    extrapolationsSeries <- c(extrapolationsSeries, list(extrapolations))
  }

  return(extrapolationsSeries)
}

solve <- function(input) {
  histories <- parseInput(input)
  res <- 0

  for (history in histories) {
    extrapolationsSeries <- calculateExtrapolations(history)

    pastPrediction <- 0
    for (i in length(extrapolationsSeries):1) {
      pastPrediction <- unlist(extrapolationsSeries[i])[1] - pastPrediction
    }

    res <- res + pastPrediction
  }

  return(res)
}

input <- readLines("input.txt")
print(solve(input))
