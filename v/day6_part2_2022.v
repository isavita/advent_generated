import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    buffer := input.trim_space()

    packet_marker_pos := find_marker_position(buffer, 4)
    message_marker_pos := find_marker_position(buffer, 14)

    println('First start-of-packet marker after character: $packet_marker_pos')
    println('First start-of-message marker after character: $message_marker_pos')
}

fn find_marker_position(buffer string, length int) int {
    for i in length - 1 .. buffer.len {
        substring := buffer[i - length + 1 .. i + 1]
        if is_unique(substring) {
            return i + 1
        }
    }
    return -1 // In case no marker is found
}

fn is_unique(s string) bool {
    mut char_set := map[rune]int{}
    for c in s {
        char_set[c]++
        if char_set[c] > 1 {
            return false
        }
    }
    return true
}