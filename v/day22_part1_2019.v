import os

fn main() {
    instructions := os.read_lines('input.txt') or { panic(err) }
    card_position := 2019
    deck_size := 10007

    mut position := card_position
    for instruction in instructions {
        if instruction == 'deal into new stack' {
            position = deck_size - 1 - position
        } else if instruction.starts_with('cut ') {
            n := instruction.split(' ')[1].int()
            position = (position - n + deck_size) % deck_size
        } else if instruction.starts_with('deal with increment ') {
            n := instruction.split(' ')[3].int()
            position = (position * n) % deck_size
        }
    }

    println(position)
}