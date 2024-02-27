
# solution.awk

BEGIN {
    FS = ","
    while ((getline < "input.txt") > 0) {
        for (i = 1; i <= NF; i++) {
            if (i == NF) {
                lastSpoken = $i
            } else {
                spoken[$i] = i
            }
        }
    }

    for (turn = NF + 1; turn <= 30000000; turn++) {
        if (lastSpoken in spoken) {
            nextNumber = turn - 1 - spoken[lastSpoken]
        } else {
            nextNumber = 0
        }
        spoken[lastSpoken] = turn - 1
        lastSpoken = nextNumber
    }

    print lastSpoken
}
