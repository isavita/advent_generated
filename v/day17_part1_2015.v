import os

fn main() {
    containers := read_input('input.txt')
    target := 150
    count := count_combinations(containers, target)
    println(count)
}

fn read_input(filename string) []int {
    contents := os.read_file(filename) or { panic(err) }
    return contents.split('\n').filter(it != '').map(it.int())
}

fn count_combinations(containers []int, target int) int {
    return count_recursive(containers, target, 0)
}

fn count_recursive(containers []int, target int, index int) int {
    if target == 0 {
        return 1
    }
    if target < 0 || index >= containers.len {
        return 0
    }
    return count_recursive(containers, target - containers[index], index + 1) +
           count_recursive(containers, target, index + 1)
}