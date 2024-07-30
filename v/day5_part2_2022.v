module main

import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    parts := input.split('\n\n')
    lines := parts[0].split('\n')
    mut stacks := [][]u8{len: (lines[0].len + 1) / 4}

    for line in lines {
        for i, b in line.bytes() {
            if b >= `A` && b <= `Z` {
                stacks[(i - 1) / 4] << b
            }
        }
    }

    steps := parts[1].split('\n')
    println(move(mut stacks, steps))
}

fn move(mut st [][]u8, steps []string) string {
    mut stacks := [][]u8{len: st.len}
    for i in 0 .. st.len {
        stacks[i] = st[i].reverse()
    }

    for step in steps {
        n, from, to := parse_step(step)
        stacks[to] << stacks[from][stacks[from].len - n..]
        stacks[from] = stacks[from][0..stacks[from].len - n]
    }

    mut result := []u8{len: stacks.len}
    for i in 0 .. stacks.len {
        result[i] = stacks[i][stacks[i].len - 1]
    }
    return result.bytestr()
}

fn parse_step(step string) (int, int, int) {
    parts := step.split(' ')
    return parts[1].int(), parts[3].int() - 1, parts[5].int() - 1
}