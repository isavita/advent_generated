
import os

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    print(data)
}
