import os

fn main() {
    input := os.read_file('input.txt') or { return }
    n := input.trim_space().int()

    mut scoreboard := [3, 7]
    mut elf1 := 0
    mut elf2 := 1

    for scoreboard.len < n + 10 {
        new_score := scoreboard[elf1] + scoreboard[elf2]
        if new_score >= 10 {
            scoreboard << new_score / 10
        }
        scoreboard << new_score % 10

        elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.len
        elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.len
    }

    for i in n .. n + 10 {
        print(scoreboard[i])
    }
    println('')
}