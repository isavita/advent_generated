
vals <- as.numeric(readLines("input.txt"))

prevSum <- vals[1] + vals[2] + vals[3]
count <- 0
for (i in 4:length(vals)) {
    currSum <- vals[i-2] + vals[i-1] + vals[i]
    if (currSum > prevSum) {
        count <- count + 1
    }
    prevSum <- currSum
}

cat(count, "\n")
