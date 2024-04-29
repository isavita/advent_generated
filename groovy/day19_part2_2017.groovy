def file = new File("input.txt")
def grid = file.collect { it.toCharArray() }

def x = 0, y = 0
for (i in 0..<grid[0].size()) {
    if (grid[0][i] == '|'.charAt(0)) {
        x = i
        break
    }
}

def dx = 0, dy = 1
def steps = 0

while (true) {
    if (x < 0 || x >= grid[0].size() || y < 0 || y >= grid.size()) {
        break
    }

    def cell = grid[y][x]

    if (cell == ' '.charAt(0)) {
        break
    }

    steps++

    if (cell == '+'.charAt(0)) {
        if (dx == 0) {
            if (x > 0 && (grid[y][x-1] == '-' || (grid[y][x-1] >= 'A'.charAt(0) && grid[y][x-1] <= 'Z'.charAt(0)))) {
                dx = -1; dy = 0
            } else {
                dx = 1; dy = 0
            }
        } else {
            if (y > 0 && (grid[y-1][x] == '|' || (grid[y-1][x] >= 'A'.charAt(0) && grid[y-1][x] <= 'Z'.charAt(0)))) {
                dx = 0; dy = -1
            } else {
                dx = 0; dy = 1
            }
        }
    }

    x += dx
    y += dy
}

println steps