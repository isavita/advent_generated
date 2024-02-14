

data <- readLines("input.txt")
rules <- list()
scanningRules <- TRUE
errorRate <- 0

reRule <- "^([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$"

isValidForAnyRule <- function(value, rules) {
  for (rule in rules) {
    if (value >= rule$ranges[[1]][1] && value <= rule$ranges[[1]][2] || 
        value >= rule$ranges[[2]][1] && value <= rule$ranges[[2]][2]) {
      return(TRUE)
    }
  }
  return(FALSE)
}

for (line in data) {
  if (line == "") {
    next
  }
  if (grepl("^your ticket:|^nearby tickets:", line)) {
    scanningRules <- FALSE
    next
  }
  if (scanningRules) {
    matches <- regmatches(line, regexec(reRule, line))
    if (length(matches[[1]]) > 1) {
      name <- matches[[1]][2]
      range1 <- c(as.integer(matches[[1]][3]), as.integer(matches[[1]][4]))
      range2 <- c(as.integer(matches[[1]][5]), as.integer(matches[[1]][6]))
      rules[[length(rules) + 1]] <- list(name = name, ranges = list(range1, range2))
    }
  } else {
    values <- unlist(strsplit(line, ","))
    for (value in values) {
      val <- as.integer(value)
      if (!isValidForAnyRule(val, rules)) {
        errorRate <- errorRate + val
      }
    }
  }
}

print(errorRate)

