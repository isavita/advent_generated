File inputFile = new File("input.txt")
List<String> instructions = inputFile.readLines()

def tiles = [:]
def directions = [
    'e': [2, 0],
    'se': [1, -1],
    'sw': [-1, -1],
    'w': [-2, 0],
    'nw': [-1, 1],
    'ne': [1, 1]
]

instructions.each { instruction ->
    def coord = [0, 0]
    def i = 0
    while (i < instruction.size()) {
        def dir
        if (instruction[i] == 'e' || instruction[i] == 'w') {
            dir = instruction[i]
            i++
        } else {
            dir = instruction[i..i+1]
            i += 2
        }
        def change = directions[dir]
        coord[0] += change[0]
        coord[1] += change[1]
    }
    def key = coord.toString()
    tiles[key] = !tiles.containsKey(key) || !tiles[key]
}

def blackTiles = tiles.count { it.value }

println blackTiles