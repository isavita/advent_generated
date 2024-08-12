BEGIN {
    getline input < "input.txt"
    split(input, inputSequence, "")
    inputLen = length(input)
    scoreboard[0] = 3
    scoreboard[1] = 7
    elf1 = 0
    elf2 = 1
    lenScoreboard = 2

    while (1) {
        newScore = scoreboard[elf1] + scoreboard[elf2]
        if (newScore >= 10) {
            scoreboard[lenScoreboard++] = int(newScore / 10)
            if (checkSequence(lenScoreboard, inputSequence, inputLen)) break
        }
        scoreboard[lenScoreboard++] = newScore % 10
        if (checkSequence(lenScoreboard, inputSequence, inputLen)) break
        elf1 = (elf1 + scoreboard[elf1] + 1) % lenScoreboard
        elf2 = (elf2 + scoreboard[elf2] + 1) % lenScoreboard
    }
    print lenScoreboard - inputLen
}

function checkSequence(lenScoreboard, inputSequence, inputLen) {
    if (lenScoreboard < inputLen) return 0
    start = lenScoreboard - inputLen
    for (i = 1; i <= inputLen; i++) {
        if (scoreboard[start + i - 1] != inputSequence[i]) return 0
    }
    return 1
}