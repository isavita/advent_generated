
import os

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    lines := txt.split_into_lines()
    if lines.len == 0 { panic('empty input') }

    mut patterns := []string{}
    for p in lines[0].split(',') {
        s := p.trim_space()
        if s != '' { patterns << s }
    }

    mut total := i64(0)
    for line in lines[2..] {
        design := line.trim_space()
        if design == '' { continue }
        n := design.len
        mut dp := []i64{len: n + 1, init: 0}
        dp[0] = 1
        for i := 1; i <= n; i++ {
            for pat in patterns {
                lp := pat.len
                if i >= lp && design[i - lp..i] == pat {
                    dp[i] += dp[i - lp]
                }
            }
        }
        total += dp[n]
    }
    println(total)
}
