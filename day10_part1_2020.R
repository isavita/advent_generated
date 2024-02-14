
adapters <- as.numeric(readLines("input.txt"))
adapters <- c(0, adapters, max(adapters) + 3)

diffs <- diff(sort(adapters))
count_1 <- sum(diffs == 1)
count_3 <- sum(diffs == 3)

result <- count_1 * count_3
print(result)
