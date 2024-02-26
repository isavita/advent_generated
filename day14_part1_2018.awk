
{input = $1}

BEGIN {
    scoreboard = "37"
    elf1 = 0
    elf2 = 1
}

{
    while (length(scoreboard) < input + 10) {
        new_recipe = substr(scoreboard, elf1+1, 1) + substr(scoreboard, elf2+1, 1)
        scoreboard = scoreboard new_recipe
        elf1 = (elf1 + 1 + int(substr(scoreboard, elf1+1, 1))) % length(scoreboard)
        elf2 = (elf2 + 1 + int(substr(scoreboard, elf2+1, 1))) % length(scoreboard)
    }
    
    print substr(scoreboard, input+1, 10)
}
