
import os
import strconv

fn main() {
    raw := os.read_file('input.txt') or { panic('missing input.txt') }
    mut nums := raw.trim_space().split_into_lines()
    bit_len := nums[0].len

    mut oxy := nums.clone()
    mut bit := 0
    for oxy.len > 1 && bit < bit_len {
        mut cnt := 0
        for s in oxy {
            if s[bit] == `1` { cnt++ }
        }
        keep := if cnt >= oxy.len - cnt { `1` } else { `0` }
        oxy = oxy.filter(it[bit] == keep)
        bit++
    }

    mut co2 := nums.clone()
    bit = 0
    for co2.len > 1 && bit < bit_len {
        mut cnt := 0
        for s in co2 {
            if s[bit] == `1` { cnt++ }
        }
        keep := if cnt < co2.len - cnt { `1` } else { `0` }
        co2 = co2.filter(it[bit] == keep)
        bit++
    }

    o := strconv.parse_int(oxy[0], 2, 64) or { 0 }
    c := strconv.parse_int(co2[0], 2, 64) or { 0 }
    println(o * c)
}
