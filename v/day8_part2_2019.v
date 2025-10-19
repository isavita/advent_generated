
import os

fn main() {
	data := os.read_file('input.txt') or { panic(err) }
	width := 25
	height := 6
	size := width * height
	mut img := []u8{len: size, init: `2`}

	for i := 0; i + size <= data.len; i += size {
		for j in 0 .. size {
			if img[j] == `2` {
				img[j] = data[i + j]
			}
		}
	}

	for y in 0 .. height {
		mut line := []u8{}
		for x in 0 .. width {
			line << if img[y * width + x] == `0` { ` ` } else { `#` }
		}
		println(line.bytestr())
	}
}
