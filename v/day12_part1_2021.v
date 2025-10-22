
import os

fn dfs(current string, visited map[string]bool, caves map[string][]string) int {
    if current == 'end' {
        return 1
    }
    mut count := 0
    for next in caves[current] {
        if visited[next] && next[0] >= `a` && next[0] <= `z` {
            continue
        }
        mut v2 := visited.clone()
        v2[next] = true
        count += dfs(next, v2, caves)
    }
    return count
}

fn main() {
    text := os.read_file('input.txt') or { panic('missing input.txt') }
    mut caves := map[string][]string{}
    for line in text.split_into_lines() {
        parts := line.split('-')
        a, b := parts[0], parts[1]
        caves[a] << b
        caves[b] << a
    }
    println(dfs('start', {'start': true}, caves))
}
