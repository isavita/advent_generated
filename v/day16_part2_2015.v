import os

fn main() {
    input := os.read_file("input.txt") or { panic(err) }
    res := aunt_sue(input)
    println(res)
}

const target_sue = {
    'children': 3,
    'cats': 7,
    'samoyeds': 2,
    'pomeranians': 3,
    'akitas': 0,
    'vizslas': 0,
    'goldfish': 5,
    'trees': 3,
    'cars': 2,
    'perfumes': 1,
}

fn aunt_sue(input string) int {
    for line in input.split('\n') {
        if line == '' { continue }
        parts := line.split(' ')
        sue_num := parts[1].replace(':', '').int()
        mut readings_map := map[string]int{}

        for i in 2 .. parts.len {
            if i % 2 == 0 {
                key := parts[i].replace(':', '')
                value := parts[i + 1].int()
                readings_map[key] = value
            }
        }

        mut all_rules_matched := true
        for check in ['cats', 'trees'] {
            if scan_count := readings_map[check] {
                if scan_count <= target_sue[check] {
                    all_rules_matched = false
                }
                readings_map.delete(check)
            }
        }
        for check in ['pomeranians', 'goldfish'] {
            if scan_count := readings_map[check] {
                if scan_count >= target_sue[check] {
                    all_rules_matched = false
                }
                readings_map.delete(check)
            }
        }
        for thing, amount in readings_map {
            if target_sue[thing] != amount {
                all_rules_matched = false
            }
        }
        if all_rules_matched {
            return sue_num
        }
    }
    panic('expect return from loop')
}