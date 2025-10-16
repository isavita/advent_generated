
import os

fn supports_ssl(ip string) bool {
    mut bracket_contents := []string{}
    mut in_bracket := false
    mut current := []u8{}
    for c in ip {
        if c == `[` {
            in_bracket = true
            current.clear()
        } else if c == `]` {
            in_bracket = false
            bracket_contents << current.bytestr()
            current.clear()
        } else {
            current << c
        }
    }
    mut outside := []u8{}
    in_bracket = false
    for c in ip {
        if c == `[` {
            in_bracket = true
        } else if c == `]` {
            in_bracket = false
        } else if !in_bracket {
            outside << c
        }
    }
    for i := 0; i <= outside.len - 3; i++ {
        if outside[i] == outside[i + 2] && outside[i] != outside[i + 1] {
            bab := [outside[i + 1], outside[i], outside[i + 1]].bytestr()
            for b in bracket_contents {
                if b.contains(bab) {
                    return true
                }
            }
        }
    }
    return false
}

fn main() {
    text := os.read_file('input.txt') or { panic(err) }
    mut ssl_count := 0
    for line in text.split_into_lines() {
        if supports_ssl(line) {
            ssl_count++
        }
    }
    println(ssl_count)
}
