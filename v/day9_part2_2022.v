
import os

struct Point {
    x int
    y int
}

enum Dir {
    n
    e
    s
    w
}

fn abs(a int) int {
    return if a < 0 { -a } else { a }
}

fn sign(a int) int {
    return if a == 0 { 0 } else { if a > 0 { 1 } else { -1 } }
}

fn (a Point) + (b Point) Point {
    return Point{a.x + b.x, a.y + b.y}
}

fn dir_to_point(d Dir) Point {
    return match d {
        .n { Point{0, 1} }
        .e { Point{1, 0} }
        .s { Point{0, -1} }
        .w { Point{-1, 0} }
    }
}

fn dir_from_byte(b byte) Dir {
    return match b {
        `N`, `U`, `^` { .n }
        `E`, `R`, `>` { .e }
        `S`, `D`, `v` { .s }
        `W`, `L`, `<` { .w }
        else { .n }
    }
}

fn next(head Point, tail Point) Point {
    if abs(head.x - tail.x) <= 1 && abs(head.y - tail.y) <= 1 {
        return tail
    }
    return tail + Point{sign(head.x - tail.x), sign(head.y - tail.y)}
}

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    mut rope := [Point{0, 0}].repeat(10)
    mut visited := map[string]bool{}
    for line in txt.split_into_lines() {
        b := line[0]
        n := line[2..].int()
        d := dir_from_byte(b)
        for _ in 0 .. n {
            rope[0] = rope[0] + dir_to_point(d)
            for j in 1 .. rope.len {
                rope[j] = next(rope[j - 1], rope[j])
            }
            visited['${rope[9].x},${rope[9].y}'] = true
        }
    }
    println(visited.len)
}
