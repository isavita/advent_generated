
import os

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    mut left := []int{}
    mut right := []int{}
    for line in data.split_into_lines() {
        nums := line.split(' ')
        if nums.len < 2 { continue }
        left << nums[0].int()
        right << nums[nums.len-1].int()
    }
    left.sort()
    right.sort()
    mut total := i64(0)
    for i in 0 .. left.len {
        total += if left[i] > right[i] { left[i] - right[i] } else { right[i] - left[i] }
    }
    println(total)
}
