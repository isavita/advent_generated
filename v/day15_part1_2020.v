import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    starting_numbers := input.split(',').map(it.int())

    mut spoken := map[int]int{}
    mut last_spoken := 0

    for i in 0 .. starting_numbers.len {
        if i == starting_numbers.len - 1 {
            last_spoken = starting_numbers[i]
        } else {
            spoken[starting_numbers[i]] = i + 1
        }
    }

    for turn in starting_numbers.len + 1 .. 2021 {
        next_spoken := if spoken[last_spoken] == 0 { 0 } else { turn - 1 - spoken[last_spoken] }
        spoken[last_spoken] = turn - 1
        last_spoken = next_spoken
    }

    println(last_spoken)
}