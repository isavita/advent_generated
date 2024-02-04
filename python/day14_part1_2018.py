
with open("input.txt", "r") as file:
    input = int(file.readline())

scoreboard = [3, 7]
elf1, elf2 = 0, 1

while len(scoreboard) < input + 10:
    new_score = scoreboard[elf1] + scoreboard[elf2]
    if new_score >= 10:
        scoreboard.append(new_score // 10)
    scoreboard.append(new_score % 10)

    elf1 = (elf1 + scoreboard[elf1] + 1) % len(scoreboard)
    elf2 = (elf2 + scoreboard[elf2] + 1) % len(scoreboard)

for i in range(input, input + 10):
    print(scoreboard[i], end="")
print()
