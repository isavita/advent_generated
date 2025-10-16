
import os

fn is_digit(c byte) bool {
    return c >= `0` && c <= `9`
}

fn is_xdigit(c byte) bool {
    return (c >= `0` && c <= `9`) || (c >= `a` && c <= `f`) || (c >= `A` && c <= `F`)
}

fn atoi(s string) int {
    return s.int()
}

fn valid_year(s string, min int, max int) bool {
    if s.len != 4 { return false }
    y := s.int()
    return y >= min && y <= max
}

fn valid_hgt(s string) bool {
    if s.ends_with('cm') {
        h := s[..s.len - 2].int()
        return h >= 150 && h <= 193
    }
    if s.ends_with('in') {
        h := s[..s.len - 2].int()
        return h >= 59 && h <= 76
    }
    return false
}

fn valid_hcl(s string) bool {
    if s.len != 7 || s[0] != `#` { return false }
    for c in s[1..] {
        if !is_xdigit(c) { return false }
    }
    return true
}

fn valid_ecl(s string) bool {
    return s in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']
}

fn valid_pid(s string) bool {
    if s.len != 9 { return false }
    for c in s {
        if !is_digit(c) { return false }
    }
    return true
}

fn main() {
    txt := os.read_file('input.txt') or { panic('missing') }
    mut valid := 0
    for block in txt.split('\n\n') {
        fields := block.replace('\n', ' ').split(' ')
        mut ok := 0
        for f in fields {
            parts := f.split(':')
            if parts.len != 2 { continue }
            key := parts[0]
            val := parts[1]
            match key {
                'byr' { if valid_year(val, 1920, 2002) { ok++ } }
                'iyr' { if valid_year(val, 2010, 2020) { ok++ } }
                'eyr' { if valid_year(val, 2020, 2030) { ok++ } }
                'hgt' { if valid_hgt(val) { ok++ } }
                'hcl' { if valid_hcl(val) { ok++ } }
                'ecl' { if valid_ecl(val) { ok++ } }
                'pid' { if valid_pid(val) { ok++ } }
                else {}
            }
        }
        if ok == 7 { valid++ }
    }
    println(valid)
}
