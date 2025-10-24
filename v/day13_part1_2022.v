
import os
import strconv

enum Order { right wrong equal }

fn parse(s string) []string {
    mut res := []string{}
    mut depth := 0
    mut start := 0
    for i, ch in s {
        if ch == `[` { depth++ } else if ch == `]` { depth-- }
        if depth == 1 && ch == `,` {
            res << s[start + 1..i]
            start = i
        }
    }
    if start < s.len - 1 { res << s[start + 1..s.len - 1] }
    return res
}

fn cmp(left string, right string) Order {
    if left.len == 0 && right.len == 0 { return .equal }
    if left.len == 0 { return .right }
    if right.len == 0 { return .wrong }
    if left[0] != `[` && right[0] != `[` {
        l := strconv.atoi(left) or { 0 }
        r := strconv.atoi(right) or { 0 }
        return if l < r { .right } else if l > r { .wrong } else { .equal }
    }
    if left[0] == `[` && right[0] == `[` {
        mut l := parse(left)
        mut r := parse(right)
        for i in 0 .. l.len {
            if i >= r.len { return .wrong }
            match cmp(l[i], r[i]) {
                .right { return .right }
                .wrong { return .wrong }
                .equal {}
            }
        }
        return if l.len < r.len { .right } else { .equal }
    }
    l := if left[0] == `[` { left } else { '[$left]' }
    r := if right[0] == `[` { right } else { '[$right]' }
    return cmp(l, r)
}

fn main() {
    txt := os.read_file('input.txt') or { return }
    pairs := txt.split('\n\n')
    mut sum := 0
    for i, pair in pairs {
        lines := pair.split_into_lines()
        if lines.len >= 2 && cmp(lines[0], lines[1]) == .right {
            sum += i + 1
        }
    }
    println(sum)
}
