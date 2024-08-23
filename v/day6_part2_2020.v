import os

fn main() {
    input_file := 'input.txt'
    if !os.exists(input_file) {
        eprintln('File not found: $input_file')
        return
    }

    content := os.read_file(input_file) or {
        eprintln('Error reading file: $input_file')
        return
    }

    groups := content.split('\n\n')
    mut total_count := 0

    for group in groups {
        mut question_counts := map[string]int{}
        people := group.split('\n').filter(it != '')

        for person in people {
            for question in person {
                question_counts[question.ascii_str()]++
            }
        }

        for count in question_counts.values() {
            if count == people.len {
                total_count++
            }
        }
    }

    println(total_count)
}