import crypto.md5
import os

fn main() {
    secret_key := os.read_file('input.txt') or { panic('Failed to read file') }.trim_space()
    
    five_zeroes_result := find_lowest_number(secret_key, 5)
    six_zeroes_result := find_lowest_number(secret_key, 6)

    println('Lowest number for five zeroes: $five_zeroes_result')
    println('Lowest number for six zeroes: $six_zeroes_result')
}

fn find_lowest_number(secret_key string, zero_count int) int {
    prefix := '0'.repeat(zero_count)
    mut number := 0

    for {
        hash_input := '$secret_key$number'
        hash := md5.sum(hash_input.bytes())
        if hash.hex().starts_with(prefix) {
            return number
        }
        number++
    }
    
    return -1 // This return is for safety; logically it should never reach here.
}