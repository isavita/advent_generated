class Tile {
    int id
    List<String> data = []
}

def lines = new File('input.txt').readLines()
def tiles = []
def current = null

lines.each { line ->
    if (line.startsWith('Tile')) {
        if (current != null) tiles << current
        def m = line =~ /Tile (\d+):/
        int id = m[0][1].toInteger()
        current = new Tile(id: id)
    } else if (line.trim()) {
        if (current != null) current.data << line
    }
}
if (current != null) tiles << current

def borderCounts = [:].withDefault {0}
def getBorder = { Tile t, int idx ->
    switch (idx) {
        case 0:
            return t.data[0]
        case 1:
            return t.data[-1]
        case 2:
            def sb = new StringBuilder()
            for (int i = 0; i < t.data.size(); i++) sb.append(t.data[i].charAt(0))
            return sb.toString()
        default:
            def sb = new StringBuilder()
            for (int i = 0; i < t.data.size(); i++) sb.append(t.data[i].charAt(9))
            return sb.toString()
    }
}

// Count all borders including reversed
tiles.each { tile ->
    for (int b = 0; b < 4; b++) {
        String border = getBorder(tile, b)
        borderCounts[border] = (borderCounts[border] ?: 0) + 1
        String rev = border.reverse()
        borderCounts[rev] = (borderCounts[rev] ?: 0) + 1
    }
}

// Compute result by identifying corner tiles
long result = 1L
int cornerCount = 0
tiles.each { tile ->
    int uniqueEdges = 0
    for (int b = 0; b < 4; b++) {
        String border = getBorder(tile, b)
        if ((borderCounts[border] ?: 0) == 1) uniqueEdges++
    }
    if (uniqueEdges == 2) {
        result *= tile.id
        cornerCount++
    }
}

println result