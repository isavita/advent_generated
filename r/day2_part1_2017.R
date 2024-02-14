
input <- as.matrix(read.table("input.txt"))

checksum <- sum(apply(input, 1, function(x) max(x) - min(x)))

print(checksum)
