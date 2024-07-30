module main

import os
import crypto.md5

fn main() {
    door_id := os.read_file('input.txt') or { panic(err) }.trim_space()
    password := find_password(door_id)
    println(password)
}

fn find_password(door_id string) string {
    mut password := []u8{len: 8, init: 0}
    mut filled_positions := 0
    mut found := []bool{len: 8, init: false}

    for i := 0; filled_positions < 8; i++ {
        hash := md5_hash(door_id + i.str())
        if hash.starts_with('00000') {
            pos := hash[5] - `0`
            if pos >= 0 && pos < 8 && !found[pos] {
                found[pos] = true
                password[pos] = hash[6]
                filled_positions++
            }
        }
    }
    return password.bytestr()
}

fn md5_hash(input string) string {
    return md5.sum(input.bytes()).hex()
}