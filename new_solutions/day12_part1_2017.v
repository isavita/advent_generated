import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    mut connections := map[int][]int{}

    for line in input.split('\n') {
        if line.trim_space() == '' { continue }
        parts := line.split(' <-> ')
        program := parts[0].int()
        connected := parts[1].split(', ').map(it.int())
        connections[program] = connected
    }

    mut visited := map[int]bool{}
    count := count_group(0, connections, mut visited)
    println(count)
}

fn count_group(program int, connections map[int][]int, mut visited map[int]bool) int {
    if visited[program] { return 0 }
    visited[program] = true
    mut count := 1
    for connected in connections[program] {
        count += count_group(connected, connections, mut visited)
    }
    return count
}