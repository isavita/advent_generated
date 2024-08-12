BEGIN {
    getline < "input.txt"
    split($0, banks, " ")
    for (i in banks) banks[i] = int(banks[i])
    
    seen[""] = 1
    cycles = 0

    while (1) {
        state = ""
        for (i in banks) state = state banks[i] " "
        if (seen[state]) break
        seen[state] = 1

        maxIndex = 1
        for (i = 2; i <= length(banks); i++) {
            if (banks[i] > banks[maxIndex]) maxIndex = i
        }

        blocks = banks[maxIndex]
        banks[maxIndex] = 0
        for (i = 1; i <= blocks; i++) {
            banks[(maxIndex + i - 1) % length(banks) + 1]++
        }

        cycles++
    }

    print "It takes", cycles, "redistribution cycles to reach a repeated configuration."
}