
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")

var grid = []
var moves = ""

for (line in lines) {
    var trimmed = line.trim()
    if (trimmed.count > 0) {
        if (trimmed[0] == "#") {
            var row = []
            for (i in 0...trimmed.count) {
                row.add(trimmed[i])
            }
            grid.add(row)
        } else {
            moves = moves + trimmed
        }
    }
}

var rr = 0
var rc = 0
var R = grid.count
var C = grid[0].count

for (r in 0...R) {
    for (c in 0...C) {
        if (grid[r][c] == "@") {
            rr = r
            rc = c
        }
    }
}

for (i in 0...moves.count) {
    var move = moves[i]
    var dr = 0
    var dc = 0
    if (move == "^") {
        dr = -1
    } else if (move == "v") {
        dr = 1
    } else if (move == "<") {
        dc = -1
    } else if (move == ">") {
        dc = 1
    } else {
        continue
    }

    var tr = rr + dr
    var tc = rc + dc
    while (grid[tr][tc] == "O") {
        tr = tr + dr
        tc = tc + dc
    }

    if (grid[tr][tc] == ".") {
        if (grid[rr + dr][rc + dc] == "O") {
            grid[tr][tc] = "O"
        }
        grid[rr][rc] = "."
        rr = rr + dr
        rc = rc + dc
        grid[rr][rc] = "@"
    }
}

var s = 0
for (r in 0...R) {
    for (c in 0...C) {
        if (grid[r][c] == "O") {
            s = s + r * 100 + c
        }
    }
}

System.print(s)
