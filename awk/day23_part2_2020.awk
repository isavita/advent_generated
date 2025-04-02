
#!/usr/bin/awk -f

BEGIN {
    totalCups = 1000000
    totalMoves = 10000000

    if ((getline input < "input.txt") <= 0) {
         print "Error: Cannot read input.txt" > "/dev/stderr"
         exit 1
    }
    close("input.txt")

    inputLen = length(input)
    lastCup = 0
    firstCup = 0

    for (i = 1; i <= inputLen; i++) {
        cup = substr(input, i, 1) + 0
        if (i == 1) {
            firstCup = cup
        } else {
            cups[lastCup] = cup
        }
        lastCup = cup
    }

    for (i = inputLen + 1; i <= totalCups; i++) {
        cups[lastCup] = i
        lastCup = i
    }
    cups[lastCup] = firstCup

    currentCup = firstCup

    for (move = 1; move <= totalMoves; move++) {
        pickup1 = cups[currentCup]
        pickup2 = cups[pickup1]
        pickup3 = cups[pickup2]

        cups[currentCup] = cups[pickup3]

        destinationCup = (currentCup > 1) ? currentCup - 1 : totalCups
        while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
            destinationCup = (destinationCup > 1) ? destinationCup - 1 : totalCups
        }

        cups[pickup3] = cups[destinationCup]
        cups[destinationCup] = pickup1

        currentCup = cups[currentCup]
    }

    cup1 = cups[1]
    cup2 = cups[cup1]
    printf "%d\n", cup1 * cup2

    exit
}

