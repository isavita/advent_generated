
input <- readLines("input.txt")
totalCount <- 0
groupAnswers <- list()
groupSize <- 0

for (line in input) {
  if (line == "") {
    for (count in groupAnswers) {
      if (count == groupSize) {
        totalCount <- totalCount + 1
      }
    }
    groupAnswers <- list()
    groupSize <- 0
  } else {
    groupSize <- groupSize + 1
    for (question in strsplit(line, "")[[1]]) {
      if (is.null(groupAnswers[[question]])) {
        groupAnswers[[question]] <- 1
      } else {
        groupAnswers[[question]] <- groupAnswers[[question]] + 1
      }
    }
  }
}

for (count in groupAnswers) {
  if (count == groupSize) {
    totalCount <- totalCount + 1
  }
}

print(totalCount)
