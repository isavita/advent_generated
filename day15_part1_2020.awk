
{
    split($0, startingNumbers, ",")
}
END {
    for (i = 1; i <= 2020; i++) {
        if (i - 1 < length(startingNumbers)) {
            lastNumber = startingNumbers[i]
            lastSpoken[lastNumber] = i
            nextNumber = lastNumber
        } else {
            if (lastNumber in lastSpoken && lastSpoken[lastNumber] != i - 1) {
                nextNumber = i - 1 - lastSpoken[lastNumber]
            } else {
                nextNumber = 0
            }
            lastSpoken[lastNumber] = i - 1
            lastNumber = nextNumber
        }
    }
    print lastNumber
}
