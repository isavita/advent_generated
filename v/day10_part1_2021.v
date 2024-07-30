import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    lines := input.split('\n')
    mut total_score := 0

    for line in lines {
        score := check_line(line)
        if score > 0 {
            total_score += score
        }
    }

    println(total_score)
}

fn check_line(line string) int {
    mut stack := []rune{}
    pairs := {
        `(`: `)`,
        `[`: `]`,
        `{`: `}`,
        `<`: `>`
    }
    error_scores := {
        `)`: 3,
        `]`: 57,
        `}`: 1197,
        `>`: 25137
    }

    for ch in line.runes() {
        if pairs.keys().contains(ch) {
            stack << ch
        } else if pairs.values().contains(ch) {
            if stack.len == 0 || pairs[stack.pop()] != ch {
                return error_scores[ch] or { 0 }
            }
        }
    }
    return 0
}