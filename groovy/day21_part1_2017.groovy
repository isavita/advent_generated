def rules = [:]

new File('input.txt').eachLine { line ->
    def parts = line.split(' => ')
    rules[parts[0]] = parts[1]
}

def grid = [
    '.#.',
    '..#',
    '###'
]

5.times {
    def newSize
    def subSize

    if (grid.size() % 2 == 0) {
        subSize = 2
        newSize = grid.size() * 3 / 2
    } else {
        subSize = 3
        newSize = grid.size() * 4 / 3
    }

    def newGrid = new String[newSize]
    for (int i = 0; i < newSize; i++) {
        newGrid[i] = ''
    }

    for (int y = 0; y < grid.size(); y += subSize) {
        for (int x = 0; x < grid[0].size(); x += subSize) {
            def square = []
            for (int dy = 0; dy < subSize; dy++) {
                square << grid[y + dy].substring(x, x + subSize)
            }
            def newSquare = enhance(square.join('/'), rules)
            for (int dy = 0; dy < newSquare.split('/').size(); dy++) {
                newGrid[(int) (y / subSize * (subSize + 1) + dy)] += newSquare.split('/')[dy]
            }
        }
    }
    grid = newGrid
}

def count = 0
for (row in grid) {
    for (pixel in row) {
        if (pixel == '#') {
            count++
        }
    }
}
println count

def enhance(input, rules) {
    for (int i = 0; i < 4; i++) {
        if (rules.containsKey(input)) {
            return rules[input]
        }
        input = rotate(input)
    }
    input = flip(input)
    for (int i = 0; i < 4; i++) {
        if (rules.containsKey(input)) {
            return rules[input]
        }
        input = rotate(input)
    }
    return ''
}

def rotate(input) {
    def parts = input.split('/')
    def size = parts.size()
    def newParts = new String[size]
    for (int x = 0; x < size; x++) {
        def newRow = ''
        for (int y = size - 1; y >= 0; y--) {
            newRow += parts[y].charAt(x)
        }
        newParts[x] = newRow
    }
    return newParts.join('/')
}

def flip(input) {
    def parts = input.split('/')
    for (int i = 0; i < parts.size(); i++) {
        parts[i] = reverse(parts[i])
    }
    return parts.join('/')
}

def reverse(input) {
    def runes = input.toCharArray()
    int i = 0
    int j = runes.size() - 1
    while (i < j) {
        def temp = runes[i]
        runes[i] = runes[j]
        runes[j] = temp
        i++
        j--
    }
    return new String(runes)
}