def input = new File("input.txt").text

def x = 0, y = 0
def visited = [[x, y]: 1]

input.each {
    switch (it) {
        case '^':
            y++
            break
        case 'v':
            y--
            break
        case '>':
            x++
            break
        case '<':
            x--
            break
    }
    visited[[x, y]] = 1
}

println visited.size()