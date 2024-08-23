import os

fn main() {
    mut seat_ids := []int{}
    lines := os.read_lines('input.txt') or { return }

    for line in lines {
        row := binary_partition(line[..7], `F`, `B`, 128)
        column := binary_partition(line[7..], `L`, `R`, 8)
        seat_id := row * 8 + column
        seat_ids << seat_id
    }

    seat_ids.sort()

    for i in 0 .. seat_ids.len - 1 {
        if seat_ids[i + 1] != seat_ids[i] + 1 {
            println(seat_ids[i] + 1)
            return
        }
    }
}

fn binary_partition(code string, lower_char u8, upper_char u8, range int) int {
    mut low := 0
    mut high := range - 1
    for c in code {
        mid := (low + high) / 2
        if c == lower_char {
            high = mid
        } else {
            low = mid + 1
        }
    }
    return low
}