
import os { read_file }

fn blink(stones []u64) []u64 {
    mut result := []u64{}
    for stone in stones {
        if stone == 0 {
            result << 1
        } else {
            s := stone.str()
            if s.len % 2 == 0 {
                mid := s.len / 2
                left := s[..mid].u64()
                right := s[mid..].u64()
                result << left
                result << right
            } else {
                result << stone * 2024
            }
        }
    }
    return result
}

fn main() {
    // Read input from file
    content := read_file('input.txt') or { panic('Failed to read input.txt') }
    data := content.trim_space()
    
    // Parse initial stones
    mut stones := []u64{}
    for part in data.split(' ') {
        stones << part.u64()
    }
    
    // Simulate 25 blinks
    for _ in 0 .. 25 {
        stones = blink(stones)
    }
    
    // Output the number of stones
    println(stones.len)
}
