import os

fn main() {
    input := os.read_file('input.txt') or {
        eprintln('Failed to read the file.')
        return
    }
    score := calculate_score(input)
    println(score)
}

fn calculate_score(stream string) int {
    mut total_score := 0
    mut current_score := 0
    mut in_garbage := false
    mut skip_next := false

    for c in stream {
        if skip_next {
            skip_next = false
            continue
        }
        match c {
            `!` {
                skip_next = true
            }
            `<` {
                in_garbage = true
            }
            `>` {
                in_garbage = false
            }
            `{` {
                if !in_garbage {
                    current_score++
                }
            }
            `}` {
                if !in_garbage {
                    total_score += current_score
                    current_score--
                }
            }
            else {}
        }
    }
    return total_score
}