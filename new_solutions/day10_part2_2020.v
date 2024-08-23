import os

fn main() {
    mut adapters := []int{}
    input := os.read_file('input.txt') or { return }
    for line in input.split('\n') {
        if line != '' {
            adapters << line.int()
        }
    }
    adapters.sort()
    adapters.prepend(0)
    adapters << (adapters.last() + 3)

    mut dp := []u64{len: adapters.len, init: 0}
    dp[0] = 1

    for i in 1 .. adapters.len {
        for j in 0 .. i {
            if adapters[i] - adapters[j] <= 3 {
                dp[i] += dp[j]
            }
        }
    }

    println(dp.last())
}