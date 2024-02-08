
def Neighbors8 = [[0, 1], [0, -1], [1, 0], [-1, 0], [-1, -1], [-1, 1], [1, -1], [1, 1]]

class Part {
    def xmin, xmax, y, n
    
    boolean valid(grid) {
        for (x = xmin; x <= xmax; x++) {
            Neighbors8.each { n ->
                def pt = [(x + n[0]), (y + n[1])]
                if (grid.containsKey(pt) && grid[pt] != '.' && (grid[pt] < '0' || grid[pt] > '9')) {
                    return true
                }
            }
        }
        return false
    }
}

def inputFile = new File("input.txt")
def input = inputFile.text.trim()

def grid = [:]
def parts = []
Part curr = null

input.readLines().eachWithIndex { line, y ->
    if (curr != null) {
        parts << curr
        curr = null
    }
    line.eachWithIndex { c, x ->
        grid[[x, y]] = c
        if (c >= '0' && c <= '9') {
            if (curr == null) {
                curr = new Part(xmin: x, xmax: x, y: y, n: c as Integer)
            } else {
                curr.n *= 10
                curr.n += c as Integer
                curr.xmax = x
            }
        } else if (curr != null) {
            parts << curr
            curr = null
        }
    }
}

def partsGrid = [:]
parts.eachWithIndex { p, i ->
    (p.xmin..p.xmax).each { x ->
        partsGrid[[x, p.y]] = i
    }
}

def sum = 0
grid.each { p, c ->
    if (c == '*') {
        def neighborParts = [:]
        Neighbors8.each { n ->
            def neighbor = [p[0] + n[0], p[1] + n[1]]
            if (partsGrid.containsKey(neighbor)) {
                neighborParts[partsGrid[neighbor]] = true
            }
        }
        if (neighborParts.size() == 2) {
            def prod = 1
            neighborParts.each { i, _ ->
                prod *= parts[i].n
            }
            sum += prod
        }
    }
}
println sum
