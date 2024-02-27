
{
    adapters[NR] = $1
}

END {
    for (i = 1; i <= NR; i++) {
        for (j = i+1; j <= NR; j++) {
            if (adapters[i] > adapters[j]) {
                temp = adapters[i]
                adapters[i] = adapters[j]
                adapters[j] = temp
            }
        }
    }

    joltDifferences[3] = 1
    previousJoltage = 0

    for (i = 1; i <= NR; i++) {
        diff = adapters[i] - previousJoltage
        joltDifferences[diff]++
        previousJoltage = adapters[i]
    }

    product = joltDifferences[1] * joltDifferences[3]
    print product
}
