
import os

fn all_zeros(nums []int) bool {
    for n in nums {
        if n != 0 {
            return false
        }
    }
    return true
}

fn main() {
    mut res := 0
    lines := os.read_lines('input.txt') or { panic(err) }
    for line in lines {
        mut history := line.split(' ').map(it.int())
        mut levels := [][]int{}
        levels << history.clone()
        for !all_zeros(levels.last()) {
            mut next := []int{}
            last := levels.last()
            for i in 1 .. last.len {
                next << last[i] - last[i - 1]
            }
            levels << next
        }
        mut future := 0
        for i := levels.len - 1; i >= 0; i-- {
            future += levels[i].last()
        }
        res += future
    }
    println(res)
}
