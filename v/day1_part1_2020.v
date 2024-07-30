import os

fn main() {
    input_file := 'input.txt'
    mut numbers := []int{}

    // Read numbers from the file
    if os.exists(input_file) {
        lines := os.read_lines(input_file) or {
            eprintln('Error reading file: $err')
            return
        }
        for line in lines {
            numbers << line.int()
        }
    } else {
        eprintln('File not found: $input_file')
        return
    }

    // Use a map to find the two entries that sum to 2020
    mut seen := map[int]bool{}
    for number in numbers {
        complement := 2020 - number
        if seen[complement] {
            println(number * complement)
            return
        }
        seen[number] = true
    }
}