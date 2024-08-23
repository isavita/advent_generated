import os
import crypto.md5

fn main() {
    door_id := os.read_file('input.txt') or { return }
    mut password := ''
    mut index := 0

    for password.len < 8 {
        hash_input := door_id.trim_space() + index.str()
        hash := md5.sum(hash_input.bytes())
        hex_hash := hash.hex()

        if hex_hash.starts_with('00000') {
            password += hex_hash[5..6]
        }
        index++
    }

    println(password)
}