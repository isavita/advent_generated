
data <- readLines("input.txt")
nanobots <- lapply(data, function(x) {
  parts <- as.numeric(unlist(strsplit(x, "[^0-9-]+")))
  list(X = parts[2], Y = parts[3], Z = parts[4], Radius = parts[5])
})

findStrongestNanobot <- function(nanobots) {
  strongest <- nanobots[[1]]
  for (i in 2:length(nanobots)) {
    if (nanobots[[i]]$Radius > strongest$Radius) {
      strongest <- nanobots[[i]]
    }
  }
  return(strongest)
}

countNanobotsInRange <- function(nanobots, strongest) {
  count <- 0
  for (i in 1:length(nanobots)) {
    if (sum(abs(nanobots[[i]]$X - strongest$X), abs(nanobots[[i]]$Y - strongest$Y), abs(nanobots[[i]]$Z - strongest$Z)) <= strongest$Radius) {
      count <- count + 1
    }
  }
  return(count)
}

strongest <- findStrongestNanobot(nanobots)
inRangeCount <- countNanobotsInRange(nanobots, strongest)

print(inRangeCount)
