import os

struct Point {
    x int
    y int
    z int
    w int
}

fn abs(x int) int {
    return if x < 0 { -x } else { x }
}

fn manhattan_distance(a Point, b Point) int {
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.w - b.w)
}

fn dfs(point Point, points []Point, visited mut []bool) {
    for i in 0 .. points.len {
        if !visited[i] && manhattan_distance(point, points[i]) <= 3 {
            visited[i] = true
            dfs(points[i], points, mut visited)
        }
    }
}

fn count_constellations(points []Point) int {
    mut visited := []bool{len: points.len, init: false}
    mut count := 0

    for i in 0 .. points.len {
        if !visited[i] {
            visited[i] = true
            dfs(points[i], points, mut visited)
            count++
        }
    }

    return count
}

fn main() {
    input := os.read_file('input.txt') or { return }
    mut points := []Point{}

    for line in input.split('\n') {
        if line.trim_space() == '' {
            continue
        }
        coords := line.split(',')
        points << Point{
            x: coords[0].int(),
            y: coords[1].int(),
            z: coords[2].int(),
            w: coords[3].int(),
        }
    }

    println(count_constellations(points))
}