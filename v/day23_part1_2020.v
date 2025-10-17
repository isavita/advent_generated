
import os

const max_cups = 1000000

fn main() {
    input := os.read_file('input.txt') or { return }
    mut cups := []int{len: max_cups, init: 0}
    mut current := int(input[0] - `0`)
    for i := 0; i < input.len; i++ {
        cup := int(input[i] - `0`)
        next := if i < input.len - 1 { int(input[i + 1] - `0`) } else { int(input[0] - `0`) }
        cups[cup] = next
    }
    for _ in 0 .. 100 {
        pick1 := cups[current]
        pick2 := cups[pick1]
        pick3 := cups[pick2]
        cups[current] = cups[pick3]
        mut dest := current - 1
        if dest < 1 { dest = input.len }
        for dest == pick1 || dest == pick2 || dest == pick3 {
            dest--
            if dest < 1 { dest = input.len }
        }
        cups[pick3] = cups[dest]
        cups[dest] = pick1
        current = cups[current]
    }
    mut cup := cups[1]
    for cup != 1 {
        print(cup)
        cup = cups[cup]
        if cup == 1 { break }
    }
    println('')
}
