
with open("input.txt") as f:
    input = f.read().strip()

scoreboard = [3, 7]
elf1, elf2 = 0, 1
inputLen = len(input)
inputSequence = [int(i) for i in input]

while True:
    newScore = scoreboard[elf1] + scoreboard[elf2]
    if newScore >= 10:
        scoreboard.append(newScore // 10)
        if scoreboard[-inputLen:] == inputSequence:
            break
    scoreboard.append(newScore % 10)
    if scoreboard[-inputLen:] == inputSequence:
        break

    elf1 = (elf1 + scoreboard[elf1] + 1) % len(scoreboard)
    elf2 = (elf2 + scoreboard[elf2] + 1) % len(scoreboard)

print(len(scoreboard) - inputLen)
