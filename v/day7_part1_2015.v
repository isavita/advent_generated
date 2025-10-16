
import os

struct WireRule { wire string rule string }
struct MemoEntry { key string value u16 }

fn memo_dfs(wire_rules []WireRule, entry string, mut memo map[string]u16) u16 {
    if entry in memo { return memo[entry] }
    mut res := u16(0)
    if entry[0].is_digit() {
        res = u16(entry.int())
    } else {
        idx := wire_rules.index(wire_rules.filter(it.wire == entry)[0])
        rule := wire_rules[idx].rule
        parts := rule.split(' ')
        match parts.len {
            1 { res = memo_dfs(wire_rules, parts[0], mut memo) }
            2 { res = ~memo_dfs(wire_rules, parts[1], mut memo) }
            3 {
                a := memo_dfs(wire_rules, parts[0], mut memo)
                b := memo_dfs(wire_rules, parts[2], mut memo)
                res = match parts[1] {
                    'AND'    { a & b }
                    'OR'     { a | b }
                    'LSHIFT' { a << b }
                    'RSHIFT' { a >> b }
                    else { 0 }
                }
            }
            else {}
        }
    }
    memo[entry] = res
    return res
}

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    mut wire_rules := []WireRule{}
    for line in txt.split_into_lines() {
        p := line.split(' -> ')
        wire_rules << WireRule{p[1], p[0]}
    }
    mut memo := map[string]u16
    println(memo_dfs(wire_rules, 'a', mut memo))
}
