
import crypto.md5
import os

fn find_triplet(hash string) u8 {
    for i := 0; i < hash.len - 2; i++ {
        if hash[i] == hash[i+1] && hash[i] == hash[i+2] {
            return hash[i]
        }
    }
    return 0
}

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    salt := data.trim_space()
    mut keys := 0
    mut index := 0
    for keys < 64 {
        hash := md5.hexhash(salt + index.str())
        triplet := find_triplet(hash)
        if triplet != 0 {
            needle := triplet.repeat(5)
            for i := 1; i <= 1000; i++ {
                next := md5.hexhash(salt + (index + i).str())
                if next.contains(needle) {
                    keys++
                    break
                }
            }
        }
        index++
    }
    println(index - 1)
}
