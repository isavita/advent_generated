import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    decompressed_length := calculate_length(input.trim_space())
    println(decompressed_length)
}

fn calculate_length(s string) i64 {
    mut length := i64(0)
    mut i := 0
    for i < s.len {
        if s[i] == `(` {
            end_marker := s.index_after(')', i)
            marker := s[i + 1..end_marker]
            parts := marker.split('x')
            chars := parts[0].int()
            repeat := parts[1].int()
            i = end_marker + 1
            length += repeat * calculate_length(s[i..i + chars])
            i += chars
        } else {
            length++
            i++
        }
    }
    return length
}