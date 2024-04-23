readInput <- function() {
  input <- readLines("input.txt")
  return(input)
}

filterValues <- function(values, criteria) {
  for (i in 1:nchar(values[1])) {
    zeros <- sum(substr(values, i, i) == "0")
    ones <- length(values) - zeros
    keep <- criteria(zeros, ones)
    values <- values[substr(values, i, i) == keep]
    if (length(values) == 1) {
      break
    }
  }
  return(values[1])
}

main <- function() {
  values <- readInput()
  
  oxygenGeneratorRating <- filterValues(values, function(zeros, ones) {
    if (zeros > ones) {
      return("0")
    } else {
      return("1")
    }
  })
  oxygenGeneratorRatingInt <- strtoi(oxygenGeneratorRating, 2)
  
  co2ScrubberRating <- filterValues(values, function(zeros, ones) {
    if (zeros <= ones) {
      return("0")
    } else {
      return("1")
    }
  })
  co2ScrubberRatingInt <- strtoi(co2ScrubberRating, 2)
  
  print(oxygenGeneratorRatingInt * co2ScrubberRatingInt)
}

main()