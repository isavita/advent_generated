
#!/bin/bash

main() {
    awk '
    BEGIN { FS = "," }
    NR == 1 {
        for (i = 1; i < NF; i++) {
            spoken[$i] = i
        }
        lastSpoken = $NF
        startTurn = NF + 1
    }
    END {
        limit = 30000000
        for (turn = startTurn; turn <= limit; turn++) {
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
    ' input.txt
}

main
