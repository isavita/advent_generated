
import os

fn main() {
    distances := read_and_parse_input('input.txt')
    locations := get_unique_locations(distances)
    min_distance := find_shortest_route(locations, distances)
    println(min_distance)
}

fn read_and_parse_input(filename string) map[string]map[string]int {
    mut distances := map[string]map[string]int{}
    content := os.read_file(filename) or { panic(err) }
    for line in content.split_into_lines() {
        parts := line.split(' ')
        if parts.len != 5 { continue }
        from, to, dist := parts[0], parts[2], parts[4].int()
        if from !in distances { distances[from] = map[string]int{} }
        distances[from][to] = dist
        if to !in distances { distances[to] = map[string]int{} }
        distances[to][from] = dist
    }
    return distances
}

fn get_unique_locations(distances map[string]map[string]int) []string {
    mut set := map[string]bool{}
    for from, tos in distances {
        set[from] = true
        for to in tos.keys() { set[to] = true }
    }
    return set.keys()
}

fn find_shortest_route(locations []string, distances map[string]map[string]int) int {
    mut min := -1
    mut perm := locations.clone()
    permute(mut perm, 0, mut min, distances)
    return min
}

fn permute(perm mut []string, i int, min mut int, distances map[string]map[string]int) {
    if i == perm.len {
        d := route_distance(perm, distances)
        if min == -1 || d < min { min = d }
        return
    }
    for j in i .. perm.len {
        perm[i], perm[j] = perm[j], perm[i]
        permute(mut perm, i + 1, mut min, distances)
        perm[i], perm[j] = perm[j], perm[i]
    }
}

fn route_distance(route []string, distances map[string]map[string]int) int {
    mut sum := 0
    for i in 0 .. route.len - 1 {
        sum += distances[route[i]][route[i + 1]]
    }
    return sum
}
