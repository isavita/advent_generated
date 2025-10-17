
import os

const (
    max_elves = 2000
    map_size  = 1000
    n         = 1
    e         = 3
    s         = 5
    w         = 7
)

struct Elf {
mut:
    x    int
    y    int
    next_x int
    next_y int
    moving bool
}

const dirs = [
    [-1, -1], [-1, 0], [-1, 1],
    [0, 1], [1, 1], [1, 0],
    [1, -1], [0, -1]
]

fn hash(x int, y int) int {
    return (x + map_size / 2) * map_size + (y + map_size / 2)
}

fn unhash(h int) (int, int) {
    x := h / map_size - map_size / 2
    y := h % map_size - map_size / 2
    return x, y
}

fn is_occupied(x int, y int, mut m map[int]bool) bool {
    return m[hash(x, y)]
}

fn around_all_empty(mut elf Elf, mut m map[int]bool) bool {
    for i := 0; i < 8; i++ {
        if is_occupied(elf.x + dirs[i][0], elf.y + dirs[i][1], mut m) {
            return false
        }
    }
    return true
}

fn elf_in_direction(mut elf Elf, dir int, mut m map[int]bool) bool {
    for j in -1 .. 2 {
        d := (dir + j + 8) % 8
        if is_occupied(elf.x + dirs[d][0], elf.y + dirs[d][1], mut m) {
            return true
        }
    }
    return false
}

fn run(mut elves []Elf, mut m map[int]bool, curr_dir int) (bool, int) {
    mut proposes := map[int]int{}
    mut someone_moved := false

    // propose
    for mut elf in elves {
        elf.moving = false
        if around_all_empty(mut elf, mut m) {
            continue
        }

        for j in 0 .. 4 {
            dir := [n, s, w, e][(curr_dir + j) % 4]
            if elf_in_direction(mut elf, dir, mut m) {
                continue
            }
            elf.next_x = elf.x + dirs[dir][0]
            elf.next_y = elf.y + dirs[dir][1]
            proposes[hash(elf.next_x, elf.next_y)]++
            elf.moving = true
            break
        }
    }

    // move
    for mut elf in elves {
        if !elf.moving {
            continue
        }
        if proposes[hash(elf.next_x, elf.next_y)] > 1 {
            continue
        }
        // move
        m.delete(hash(elf.x, elf.y))
        elf.x = elf.next_x
        elf.y = elf.next_y
        m[hash(elf.x, elf.y)] = true
        someone_moved = true
    }

    return someone_moved, (curr_dir + 1) % 4
}

fn main() {
    content := os.read_file('input.txt') or { panic('failed to read input.txt') }
    lines := content.split_into_lines()

    mut elves := []Elf{cap: max_elves}
    mut m := map[int]bool{}

    for row, line in lines {
        for col, ch in line {
            if ch == `#` {
                elves << Elf{
                    x: row
                    y: col
                    moving: false
                }
                m[hash(row, col)] = true
            }
        }
    }

    mut curr_dir := 0
    for i := 1;; i++ {
        moved, nd := run(mut elves, mut m, curr_dir)
        curr_dir = nd
        if !moved {
            println(i)
            break
        }
    }
}
