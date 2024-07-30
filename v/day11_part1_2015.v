import os

fn main() {
    current_password := os.read_file('input.txt') or { panic('Failed to read input file') }.trim_space()
    next_password := find_next_password(current_password)
    println(next_password)
}

fn find_next_password(password string) string {
    mut new_password := increment_password(password)

    for !is_valid(new_password) {
        new_password = increment_password(new_password)
    }
    return new_password
}

fn increment_password(password string) string {
    mut chars := password.bytes()
    for i := chars.len - 1; i >= 0; i-- {
        if chars[i] == `z` {
            chars[i] = `a`
        } else {
            chars[i]++
            break
        }
    }
    return chars.bytestr()
}

fn is_valid(password string) bool {
    return has_straight(password) && !contains_forbidden(password) && has_two_pairs(password)
}

fn has_straight(password string) bool {
    for i in 0 .. password.len - 2 {
        if password[i] + 1 == password[i + 1] && password[i + 1] + 1 == password[i + 2] {
            return true
        }
    }
    return false
}

fn contains_forbidden(password string) bool {
    return password.contains('i') || password.contains('o') || password.contains('l')
}

fn has_two_pairs(password string) bool {
    mut pairs := 0
    mut i := 0
    for i < password.len - 1 {
        if password[i] == password[i + 1] {
            pairs++
            i += 2 // Skip the next character to avoid overlapping pairs
        } else {
            i++
        }
    }
    return pairs >= 2
}