
data <- read.table("input.txt", header = FALSE, sep = "-")
ranges <- data.frame(start = as.numeric(data$V1), end = as.numeric(data$V2))
ranges <- ranges[order(ranges$start), ]

findUnblockedIP <- function(ranges) {
  currentIP <- 0
  for (i in 1:nrow(ranges)) {
    if (ranges$start[i] > currentIP) {
      return(currentIP)
    }
    if (ranges$end[i] >= currentIP) {
      currentIP <- ranges$end[i] + 1
    }
  }
  return(currentIP)
}

unblockedIP <- findUnblockedIP(ranges)
print(unblockedIP)
