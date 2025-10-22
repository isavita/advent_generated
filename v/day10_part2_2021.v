
import os

fn syntax_error_score(c u8) int {
    return match c {
        `)` { 3 }
        `]` { 57 }
        `}` { 1197 }
        `>` { 25137 }
        else { 0 }
    }
}

fn autocomplete_score(s string) i64 {
    mut score := i64(0)
    for c in s {
        score *= 5
        score += match c {
            `)` { 1 }
            `]` { 2 }
            `}` { 3 }
            `>` { 4 }
            else { 0 }
        }
    }
    return score
}

fn main() {
    lines := os.read_file('input.txt')!.split_into_lines()
    mut syntax_error_score_total := 0
    mut autocomplete_scores := []i64{}

    for line in lines {
        mut stack := []u8{}
        mut corrupted := false
        for c in line {
            match c {
                `(` { stack << `)` }
                `[` { stack << `]` }
                `{` { stack << `}` }
                `<` { stack << `>` }
                else {
                    if stack.len == 0 || stack.last() != c {
                        syntax_error_score_total += syntax_error_score(c)
                        corrupted = true
                        break
                    } else {
                        stack.pop()
                    }
                }
            }
        }
        if !corrupted && stack.len > 0 {
            mut auto := ''
            for stack.len > 0 {
                auto += stack.pop().ascii_str()
            }
            autocomplete_scores << autocomplete_score(auto)
        }
    }

    println('Syntax error score: $syntax_error_score_total')

    autocomplete_scores.sort()
    middle_score := autocomplete_scores[autocomplete_scores.len / 2]
    println('Middle autocomplete score: $middle_score')
}
