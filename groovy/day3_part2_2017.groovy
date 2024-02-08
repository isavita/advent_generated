def input = new File("input.txt").text.toInteger()

def x = 0, y = 0, dx = 0, dy = -1
def grid = [:]
def sum = 0

for (int i = 1; i <= input; i++) {
    sum = 0
    for (int dx2 = -1; dx2 <= 1; dx2++) {
        for (int dy2 = -1; dy2 <= 1; dy2++) {
            sum += grid.getOrDefault([x + dx2, y + dy2], 0)
        }
    }

    if (sum == 0) {
        sum = 1
    }

    grid[[x, y]] = sum

    if (sum > input) {
        println sum
        break
    }

    if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)) {
        def tmp = dx
        dx = -dy
        dy = tmp
    }

    x += dx
    y += dy
}