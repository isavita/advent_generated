
import os

fn main() {
    data := os.read_file('input.txt') or {
        eprintln('Error reading input file')
        return
    }
    target := data.trim_space().int()

    mut houses := []i64{len: 1000000, init: 0}
    for i in 1 .. 1000000 {
        for j := i; j < 1000000; j += i {
            houses[j] += i64(i) * 10
        }
    }

    mut result := 0
    for i in 1 .. 1000000 {
        if houses[i] >= target {
            result = i
            break
        }
    }
    println(result)
}
