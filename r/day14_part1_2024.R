
input <- readLines("input.txt")
robots <- strsplit(input, " ")
robots <- lapply(robots, function(x) {
  p <- as.integer(strsplit(gsub("p=", "", x[1]), ",")[[1]])
  v <- as.integer(strsplit(gsub("v=", "", x[2]), ",")[[1]])
  c(p, v)
})
robots <- matrix(unlist(robots), ncol = 4, byrow = TRUE)
width <- 101
height <- 103

for (i in 1:100) {
  robots[, 1] <- (robots[, 1] + robots[, 3]) %% width
  robots[, 2] <- (robots[, 2] + robots[, 4]) %% height
  robots[robots[,1] < 0, 1] <- robots[robots[,1] < 0, 1] + width
  robots[robots[,2] < 0, 2] <- robots[robots[,2] < 0, 2] + height
}

q1 <- sum(robots[, 1] < 50 & robots[, 2] < 51 & (robots[,1] != 50 & robots[,2] != 51))
q2 <- sum(robots[, 1] > 50 & robots[, 2] < 51 & (robots[,1] != 50 & robots[,2] != 51))
q3 <- sum(robots[, 1] < 50 & robots[, 2] > 51 & (robots[,1] != 50 & robots[,2] != 51))
q4 <- sum(robots[, 1] > 50 & robots[, 2] > 51 & (robots[,1] != 50 & robots[,2] != 51))

cat(q1 * q2 * q3 * q4, "\n")
