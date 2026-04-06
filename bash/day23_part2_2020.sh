
#!/bin/bash

awk 'BEGIN {
    if ((getline input < "input.txt") <= 0) exit 1
    
    totalCups = 1000000
    totalMoves = 10000000
    len = length(input)

    for (i = 1; i <= len; i++) {
        cup = substr(input, i, 1) + 0
        if (i == 1) firstCup = cup
        else cups[prev] = cup
        prev = cup
    }

    for (i = len + 1; i <= totalCups; i++) {
        cups[prev] = i
        prev = i
    }
    cups[prev] = firstCup
    current = firstCup

    for (m = 1; m <= totalMoves; m++) {
        p1 = cups[current]
        p2 = cups[p1]
        p3 = cups[p2]

        cups[current] = cups[p3]

        dest = (current == 1) ? totalCups : current - 1
        while (dest == p1 || dest == p2 || dest == p3) {
            dest = (dest == 1) ? totalCups : dest - 1
        }

        cups[p3] = cups[dest]
        cups[dest] = p1
        current = cups[current]
    }

    printf "%.0f\n", cups[1] * cups[cups[1]]
}'
