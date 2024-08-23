import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    sequence := input.trim_space()

    mut scoreboard := [3, 7]
    mut elf1 := 0
    mut elf2 := 1

    for {
        new_score := scoreboard[elf1] + scoreboard[elf2]
        if new_score >= 10 {
            scoreboard << new_score / 10
            scoreboard << new_score % 10
        } else {
            scoreboard << new_score
        }

        elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.len
        elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.len

        if scoreboard.len >= sequence.len {
            if scoreboard[scoreboard.len - sequence.len..].map(it.str()).join('') == sequence {
                println(scoreboard.len - sequence.len)
                return
            }
            if scoreboard.len > sequence.len && scoreboard[scoreboard.len - sequence.len - 1..scoreboard.len - 1].map(it.str()).join('') == sequence {
                println(scoreboard.len - sequence.len - 1)
                return
            }
        }
    }
}