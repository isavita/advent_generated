
import os

fn alphabetize(s string) string {
    mut b := s.bytes()
    b.sort()
    return b.bytestr()
}

fn contains_all_chars(larger string, smaller string) bool {
    if smaller.len > larger.len {
        return false
    }
    mut i_large := 0
    for c_small in smaller {
        for i_large < larger.len && larger[i_large] < c_small {
            i_large++
        }
        if i_large >= larger.len || larger[i_large] != c_small {
            return false
        }
        i_large++
    }
    return true
}

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    lines := data.split_into_lines()
    mut total_sum := i64(0)
    for line in lines {
        parts := line.split(' ')
        mut all_patterns := []string{cap: 14}
        for i in 0..10 {
            all_patterns << alphabetize(parts[i])
        }
        mut mapping := []string{len: 10}
        mut five := []string{}
        mut six := []string{}
        for p in all_patterns {
            match p.len {
                2 { mapping[1] = p }
                4 { mapping[4] = p }
                3 { mapping[7] = p }
                7 { mapping[8] = p }
                5 { five << p }
                6 { six << p }
                else {}
            }
        }
        for p in six {
            if !contains_all_chars(p, mapping[1]) {
                mapping[6] = p
            } else if contains_all_chars(p, mapping[4]) {
                mapping[9] = p
            } else {
                mapping[0] = p
            }
        }
        for p in five {
            if contains_all_chars(p, mapping[1]) {
                mapping[3] = p
            } else if contains_all_chars(mapping[6], p) {
                mapping[5] = p
            } else {
                mapping[2] = p
            }
        }
        mut val := 0
        for i in 11..15 {
            out := alphabetize(parts[i])
            for d in 0..10 {
                if out == mapping[d] {
                    val = val*10 + d
                    break
                }
            }
        }
        total_sum += val
    }
    println(total_sum)
}
