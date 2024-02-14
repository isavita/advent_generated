
data <- readLines("input.txt")
floor <- sum(ifelse(strsplit(data, "")[[1]] == "(", 1, -1))
print(floor)
