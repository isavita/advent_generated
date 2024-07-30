import os
import crypto.md5

fn main() {
    data := os.read_file("input.txt") or { panic(err) }
    secret_key := data.trim_space()
    mut number := 0

    for {
        input := secret_key + number.str()
        hash := md5.sum(input.bytes())
        hash_string := hash.hex()

        if hash_string.starts_with("00000") {
            println(number)
            break
        }
        number++
    }
}