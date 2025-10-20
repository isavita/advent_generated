import os

fn main() {
    content := os.read_file('input.txt') or { return }
    mut sum := 0
    for line in content.split_into_lines() {
        fields := line.fields()
        for i in 0..fields.len {
            for j in 0..fields.len {
                if i != j {
                    a := fields[i].int()
                    b := fields[j].int()
                    if a % b == 0 {
                        sum += a / b
                    }
                }
            }
        }
    }
    print(sum)
}