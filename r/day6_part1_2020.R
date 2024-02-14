
lines <- readLines("input.txt")
totalCount <- 0
groupAnswers <- list()

for (line in lines) {
  if (line == "") {
    totalCount <- totalCount + length(groupAnswers)
    groupAnswers <- list()
  } else {
    for (question in strsplit(line, "")[[1]]) {
      groupAnswers[[question]] <- TRUE
    }
  }
}

totalCount <- totalCount + length(groupAnswers)
cat(totalCount, "\n")
