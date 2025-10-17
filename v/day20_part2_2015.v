
import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    target := input.int() / 11

    mut houses := []int{len: target + 1}

    for elf in 1 .. target + 1 {
        mut house := elf
        for j := 0; j < 50 && house <= target; j++ {
            houses[house] += elf
            house += elf
        }
    }

    for i, h in houses {
        if h >= target {
            println(i)
            break
        }
    }
}
