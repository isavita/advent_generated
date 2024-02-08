def keypad = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]

def x = 1
def y = 1

new File('input.txt').eachLine { line ->
    line.each {
        switch (it) {
            case 'U':
                y = Math.max(0, y - 1)
                break
            case 'D':
                y = Math.min(2, y + 1)
                break
            case 'L':
                x = Math.max(0, x - 1)
                break
            case 'R':
                x = Math.min(2, x + 1)
                break
        }
    }
    print keypad[y][x]
}

println ''