import os

fn main() {
    input_file := 'input.txt'
    if !os.exists(input_file) {
        println('File not found: $input_file')
        return
    }

    contents := os.read_file(input_file) or {
        println('Error reading file: $input_file')
        return
    }

    mut max_calories := 0
    mut current_calories := 0

    for line in contents.split('\n') {
        if line.trim_space() == '' {
            if current_calories > max_calories {
                max_calories = current_calories
            }
            current_calories = 0
        } else {
            current_calories += line.int()
        }
    }
    if current_calories > max_calories {
        max_calories = current_calories
    }

    println(max_calories)
}