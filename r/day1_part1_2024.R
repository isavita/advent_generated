
input <- read.table("input.txt", header = FALSE)
left <- sort(input[, 1])
right <- sort(input[, 2])
print(sum(abs(left - right)))
