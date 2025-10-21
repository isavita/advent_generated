
import os

struct FileSegment {
	id   int
	start int
	end   int
}

fn main() {
	line := os.read_file('input.txt') or { panic(err) }.trim_space()
	mut disk := []int{}
	mut file_id := 0
	mut is_file := true
	for c in line {
		length := int(c - `0`)
		for _ in 0 .. length {
			disk << if is_file { file_id } else { -1 }
		}
		if is_file {
			file_id++
		}
		is_file = !is_file
	}
	mut files := []FileSegment{}
	mut cur := -2
	mut start := 0
	for i, v in disk {
		if v == -1 {
			cur = -2
			continue
		}
		if v != cur {
			cur = v
			start = i
		}
		if i == disk.len - 1 || disk[i + 1] != v {
			files << FileSegment{cur, start, i}
		}
	}
	files.sort(a.id > b.id)
	for f in files {
		len := f.end - f.start + 1
		mut left := -1
		mut free := 0
		for i := 0; i < f.start; i++ {
			if disk[i] == -1 {
				if free == 0 {
					left = i
				}
				free++
				if free == len {
					break
				}
			} else {
				free = 0
				left = -1
			}
		}
		if left != -1 && free == len {
			for i in f.start .. f.end + 1 {
				disk[i] = -1
			}
			for i in left .. left + len {
				disk[i] = f.id
			}
		}
	}
	mut checksum := i64(0)
	for i, v in disk {
		if v != -1 {
			checksum += i64(i) * v
		}
	}
	println(checksum)
}
