
import os

const max = 1000

struct Position {
    x int
    y int
    risk int
}

fn main() {
    content := os.read_file('input.txt') or { panic(err) }
    lines := content.split_into_lines()
    mut grid := [][]int{len: max, init: []int{len: max}}
    mut rows := 0
    for line in lines {
        mut cols := 0
        for ch in line {
            grid[rows][cols] = int(ch - `0`)
            cols++
        }
        rows++
    }
    cols := lines[0].len
    mut dist := [][]int{len: rows, init: []int{len: cols, init: max}}
    mut pq := []Position{}
    dist[0][0] = 0
    pq << Position{0, 0, 0}
    dirs := [Position{1, 0, 0}, Position{0, 1, 0}, Position{-1, 0, 0}, Position{0, -1, 0}]
    for pq.len > 0 {
        mut idx := 0
        for i in 1 .. pq.len {
            if pq[i].risk < pq[idx].risk { idx = i }
        }
        curr := pq[idx]
        pq.delete(idx)
        if curr.x == rows - 1 && curr.y == cols - 1 {
            println(curr.risk)
            return
        }
        for d in dirs {
            nx := curr.x + d.x
            ny := curr.y + d.y
            if nx >= 0 && ny >= 0 && nx < rows && ny < cols {
                nr := curr.risk + grid[nx][ny]
                if nr < dist[nx][ny] {
                    dist[nx][ny] = nr
                    pq << Position{nx, ny, nr}
                }
            }
        }
    }
}
