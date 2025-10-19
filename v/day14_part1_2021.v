
import os

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    lines := txt.trim_space().split_into_lines()
    mut template := lines[0]
    rules := lines[2..].map(it.split(' -> '))

    for _ in 0..10 {
        mut b := []u8{cap: template.len*2}
        for i := 0; i < template.len-1; i++ {
            b << template[i]
            for r in rules {
                if template[i].ascii_str()+template[i+1].ascii_str() == r[0] {
                    b << r[1][0]
                    break
                }
            }
        }
        b << template[template.len-1]
        template = b.bytestr()
    }

    mut cnt := [256]int{}
    for c in template { cnt[c]++ }
    mut min := int(1e9)
    mut max := 0
    for v in cnt { if v > 0 { if v < min { min = v } if v > max { max = v } } }
    println(max - min)
}
