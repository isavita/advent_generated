
import "io" for File

var grid = []
for (line in File.read("input.txt").split("\n")) {
    var row = []
    for (b in line.bytes) {
        if (b >= 48 && b <= 57) row.add(b - 48)
    }
    if (row.count > 0) grid.add(row)
}

var rows = grid.count
var cols = grid[0].count

var visible = 0
var maxScore = 0

for (r in 0...rows) {
    for (c in 0...cols) {
        var h = grid[r][c]
        
        if (r == 0 || r == rows - 1 || c == 0 || c == cols - 1) {
            visible = visible + 1
        } else {
            var vis = false
            
            var blocked = false
            for (k in r-1..0) {
                if (grid[k][c] >= h) {
                    blocked = true
                    break
                }
            }
            if (!blocked) vis = true
            
            if (!vis) {
                blocked = false
                for (k in r+1...rows) {
                    if (grid[k][c] >= h) {
                        blocked = true
                        break
                    }
                }
                if (!blocked) vis = true
            }
            
            if (!vis) {
                blocked = false
                for (k in c-1..0) {
                    if (grid[r][k] >= h) {
                        blocked = true
                        break
                    }
                }
                if (!blocked) vis = true
            }
            
            if (!vis) {
                blocked = false
                for (k in c+1...cols) {
                    if (grid[r][k] >= h) {
                        blocked = true
                        break
                    }
                }
                if (!blocked) vis = true
            }
            
            if (vis) visible = visible + 1
        }
        
        var up = 0
        if (r > 0) {
            for (k in r-1..0) {
                up = up + 1
                if (grid[k][c] >= h) break
            }
        }
        
        var down = 0
        if (r < rows - 1) {
            for (k in r+1...rows) {
                down = down + 1
                if (grid[k][c] >= h) break
            }
        }
        
        var left = 0
        if (c > 0) {
            for (k in c-1..0) {
                left = left + 1
                if (grid[r][k] >= h) break
            }
        }
        
        var right = 0
        if (c < cols - 1) {
            for (k in c+1...cols) {
                right = right + 1
                if (grid[r][k] >= h) break
            }
        }
        
        var score = up * down * left * right
        if (score > maxScore) maxScore = score
    }
}

System.print(visible)
System.print(maxScore)
