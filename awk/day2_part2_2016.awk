
#!/usr/bin/awk -f

BEGIN {
    keypad["1,D"] = "3"
    keypad["2,R"] = "3"
    keypad["2,D"] = "6"
    keypad["3,U"] = "1"
    keypad["3,R"] = "4"
    keypad["3,D"] = "7"
    keypad["3,L"] = "2"
    keypad["4,L"] = "3"
    keypad["4,D"] = "8"
    keypad["5,R"] = "6"
    keypad["6,U"] = "2"
    keypad["6,R"] = "7"
    keypad["6,D"] = "A"
    keypad["6,L"] = "5"
    keypad["7,U"] = "3"
    keypad["7,R"] = "8"
    keypad["7,D"] = "B"
    keypad["7,L"] = "6"
    keypad["8,U"] = "4"
    keypad["8,R"] = "9"
    keypad["8,D"] = "C"
    keypad["8,L"] = "7"
    keypad["9,L"] = "8"
    keypad["A,U"] = "6"
    keypad["A,R"] = "B"
    keypad["B,U"] = "7"
    keypad["B,R"] = "C"
    keypad["B,D"] = "D"
    keypad["B,L"] = "A"
    keypad["C,U"] = "8"
    keypad["C,L"] = "B"
    keypad["D,U"] = "B"

    position = "5"
}

{
    for (i = 1; i <= length($0); i++) {
        move = substr($0, i, 1)
        if (keypad[position "," move]) {
            position = keypad[position "," move]
        }
    }
    code = code position
}

END {
    print code
}
