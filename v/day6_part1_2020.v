import os

fn main() {
    input_file := 'input.txt'
    if !os.exists(input_file) {
        println('File not found: $input_file')
        return
    }

    content := os.read_file(input_file) or {
        println('Error reading file: $input_file')
        return
    }

    groups := content.split('\n\n')
    mut total_count := 0

    for group in groups {
        mut questions := map[string]bool{}
        for line in group.split('\n') {
            for question in line {
                questions[question.ascii_str()] = true
            }
        }
        total_count += questions.len
    }

    println(total_count)
}