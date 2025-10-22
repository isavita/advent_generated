
import os

fn main() {
    mut sum := 0
    content := os.read_file('input.txt') or { '' }
    for line in content.split_into_lines() {
        if is_real_room(line) {
            sum += get_sector_id(line)
        }
    }
    println(sum)
}

fn is_real_room(room string) bool {
    parts := room.split('[')
    if parts.len != 2 { return false }
    checksum := parts[1][..parts[1].len - 1]
    encrypted := parts[0].split('-')
    letters := encrypted[..encrypted.len - 1].join('')
    mut counts := map[string]int{}
    for ch in letters {
        counts[ch.ascii_str()]++
    }
    mut items := []Count{}
    for k, v in counts {
        items << Count{k, v}
    }
    items.sort_with_compare(fn (a &Count, b &Count) int {
        if a.c < b.c { return 1 }
        if a.c > b.c { return -1 }
        return if a.l < b.l { -1 } else { 1 }
    })
    for i in 0 .. checksum.len {
        if checksum[i].ascii_str() != items[i].l {
            return false
        }
    }
    return true
}

fn get_sector_id(room string) int {
    parts := room.split('-')
    last := parts[parts.len - 1]
    id := last.split('[')[0].int()
    return id
}

struct Count {
    l string
    c int
}
