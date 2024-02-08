
def file = new File("input.txt")
def grid = new int[1000][1000]

file.eachLine { instruction ->
    def parts = instruction.split(" ")
    def (startX, startY) = parts[-3].tokenize(",").collect { it as int }
    def (endX, endY) = parts[-1].tokenize(",").collect { it as int }

    (startX..endX).each { x ->
        (startY..endY).each { y ->
            switch (parts[0]) {
                case "turn":
                    switch (parts[1]) {
                        case "on":
                            grid[x][y]++
                            break
                        case "off":
                            if (grid[x][y] > 0) grid[x][y]--
                            break
                    }
                    break
                case "toggle":
                    grid[x][y] += 2
                    break
            }
        }
    }
}

def brightness = grid.collectMany { it.toList() }.sum()
println brightness
