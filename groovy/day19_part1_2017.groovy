def file = new File('input.txt')
def grid = file.collect { it.toCharArray() }
def x, y = 0
for (i in 0..<grid[0].size()) {
    if (grid[0][i] == '|'.charAt(0)) {
        x = i
        break
    }
}
def dx = 0
def dy = 1
def letters = []
while (true) {
    def cell = grid[y][x]
    if (cell == ' '.charAt(0)) {
        break
    }
    if (cell >= 'A'.charAt(0) && cell <= 'Z'.charAt(0)) {
        letters << cell
    }
    if (cell == '+'.charAt(0)) {
        if (dx == 0) {
            if (x > 0 && (grid[y][x-1] == '-' || (grid[y][x-1] >= 'A'.charAt(0) && grid[y][x-1] <= 'Z'.charAt(0)))) {
                dx = -1
                dy = 0
            } else {
                dx = 1
                dy = 0
            }
        } else {
            if (y > 0 && (grid[y-1][x] == '|' || (grid[y-1][x] >= 'A'.charAt(0) && grid[y-1][x] <= 'Z'.charAt(0)))) {
                dx = 0
                dy = -1
            } else {
                dx = 0
                dy = 1
            }
        }
    }
    x += dx
    y += dy
}
println letters.join('')