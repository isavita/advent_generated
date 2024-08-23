import os

fn parse_tree(data []int, index int) (int, int) {
    child_count := data[index]
    metadata_count := data[index + 1]
    mut new_index := index + 2

    mut sum := 0
    for _ in 0 .. child_count {
        subtree_sum, updated_index := parse_tree(data, new_index)
        sum += subtree_sum
        new_index = updated_index
    }

    for _ in 0 .. metadata_count {
        sum += data[new_index]
        new_index++
    }
    return sum, new_index
}

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    numbers := input.split(' ').map(it.int())
    result, _ := parse_tree(numbers, 0)
    println(result)
}