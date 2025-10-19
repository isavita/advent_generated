
import os

fn evaluate(target i64, numbers []i64) bool {
    n := numbers.len
    if n == 0 { return false }
    if n == 1 { return numbers[0] == target }
    limit := 1 << (n - 1)
    for i := 0; i < limit; i++ {
        mut res := numbers[0]
        for j := 0; j < n - 1; j++ {
            if (i >> j) & 1 == 0 {
                res += numbers[j + 1]
            } else {
                res *= numbers[j + 1]
            }
        }
        if res == target { return true }
    }
    return false
}

fn main() {
    data := os.read_file('input.txt') or { return }
    mut sum := i64(0)
    for line in data.split_into_lines() {
        parts := line.split(':')
        if parts.len != 2 { continue }
        target := parts[0].i64()
        numbers := parts[1].fields().map(it.i64())
        if evaluate(target, numbers) {
            sum += target
        }
    }
    println(sum)
}
