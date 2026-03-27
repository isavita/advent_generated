options(scipen = 999)
input <- readLines("input.txt")
input <- input[input != ""]
hex <- gsub(".*#|\\).*", "", input)
dist <- strtoi(substr(hex, 1, 5), 16)
dirs <- as.integer(substr(hex, 6, 6))

dx <- c(1, 0, -1, 0)[dirs + 1]
dy <- c(0, 1, 0, -1)[dirs + 1]

x <- c(0, cumsum(as.numeric(dx) * dist))
y <- c(0, cumsum(as.numeric(dy) * dist))
n <- length(x)

area2 <- abs(sum(x[-n] * y[-1] - x[-1] * y[-n]))
perim <- sum(as.numeric(dist))
ans <- area2 / 2 + perim / 2 + 1

cat(ans, "\n")