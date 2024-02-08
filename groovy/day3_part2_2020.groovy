
def lines = new File('input.txt').readLines()
def width = lines[0].length()
def height = lines.size()

def countTrees = { int right, int down ->
    def trees = 0
    def x = 0
    def y = 0
    while (y < height) {
        if (lines[y][x % width] == '#') {
            trees++
        }
        x += right
        y += down
    }
    trees
}

def result = countTrees(1, 1) * countTrees(3, 1) * countTrees(5, 1) * countTrees(7, 1) * countTrees(1, 2)
println result
