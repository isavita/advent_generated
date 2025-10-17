
import os

fn main() {
	mut txt := os.read_file('input.txt') or { panic(err) }
	txt = txt.trim_space()

	mut total := 0
	for c in txt { total += int(c) - 48 }
	if total == 0 { println(0); return }

	mut disk := []int{len: total}

	mut pos := 0
	for i := 0; i < txt.len; i++ {
		len := int(txt[i]) - 48
		id := if i % 2 == 0 { i / 2 } else { -1 }
		for _ in 0..len { disk[pos] = id; pos++ }
	}

	mut l := 0
	mut r := total - 1
	for l < r {
		for l < r && disk[l] != -1 { l++ }
		for l < r && disk[r] == -1 { r-- }
		if l < r { disk[l], disk[r] = disk[r], -1; l++; r-- }
	}

	mut checksum := u64(0)
	for i, v in disk { if v != -1 { checksum += u64(i) * u64(v) } }
	println(checksum)
}
