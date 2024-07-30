import os

fn knot_hash(lengths []int) int {
    mut list := []int{cap: 256}
    for i in 0 .. 256 {
        list << i
    }
    
    mut current_position := 0
    mut skip_size := 0

    for length in lengths {
        if length > 256 {
            continue
        }
        // Reverse the relevant section
        for i in 0 .. length / 2 {
            a := (current_position + i) % 256
            b := (current_position + length - 1 - i) % 256
            list[a], list[b] = list[b], list[a]
        }
        current_position = (current_position + length + skip_size) % 256
        skip_size++
    }
    return list[0] * list[1]
}

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    lengths := input.split(',').map(it.int()).filter(it < 256)
    result := knot_hash(lengths)
    println(result)
}