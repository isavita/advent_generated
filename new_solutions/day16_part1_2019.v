import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    mut signal := input.trim_space().split('').map(it.int())
    phases := 100

    for _ in 0 .. phases {
        signal = fft(signal)
    }

    println(signal[..8].map(it.str()).join(''))
}

fn fft(input []int) []int {
    mut output := []int{len: input.len}
    for i in 0 .. input.len {
        mut sum := 0
        for j in 0 .. input.len {
            pattern_value := get_pattern_value(i, j)
            sum += input[j] * pattern_value
        }
        output[i] = abs(sum) % 10
    }
    return output
}

fn get_pattern_value(position int, index int) int {
    base_pattern := [0, 1, 0, -1]
    pattern_index := ((index + 1) / (position + 1)) % base_pattern.len
    return base_pattern[pattern_index]
}

fn abs(x int) int {
    return if x < 0 { -x } else { x }
}