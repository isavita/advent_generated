
import os

struct Coord {
    x f64
    y f64
    z f64
}

struct Point {
    pos Coord
    vel Coord
}

fn parse_input(path string) []Point {
    lines := os.read_lines(path) or { panic(err) }
    mut points := []Point{cap: lines.len}
    for line in lines {
        mut parts := []f64{}
        for token in line.replace(',', '').replace('@', '').split(' ') {
            if token.len > 0 {
                parts << token.f64()
            }
        }
        points << Point{
            pos: Coord{parts[0], parts[1], parts[2]}
            vel: Coord{parts[3], parts[4], parts[5]}
        }
    }
    return points
}

fn intersect_2d(a Point, b Point) (bool, f64, f64, f64, f64) {
    det := a.vel.x * b.vel.y - b.vel.x * a.vel.y
    if det == 0 { return false, 0, 0, 0, 0 }
    dx := b.pos.x - a.pos.x
    dy := b.pos.y - a.pos.y
    t1 := (b.vel.y * dx - b.vel.x * dy) / det
    t2 := (a.vel.y * dx - a.vel.x * dy) / det
    ix := a.pos.x + a.vel.x * t1
    iy := a.pos.y + a.vel.y * t1
    return true, ix, iy, t1, t2
}

fn main() {
    points := parse_input('input.txt')
    mut cnt := 0
    min := 200000000000000.0
    max := 400000000000000.0
    for i := 0; i < points.len; i++ {
        for j := i + 1; j < points.len; j++ {
            ok, x, y, t1, t2 := intersect_2d(points[i], points[j])
            if ok && t1 >= 0 && t2 >= 0 && x >= min && x <= max && y >= min && y <= max {
                cnt++
            }
        }
    }
    println(cnt)
}
