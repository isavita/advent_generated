def input = new File("input.txt").readLines()
def width = input[0].size()
def height = input.size()

def countTrees = { int right, int down ->
    def trees = 0
    def x = 0
    def y = 0

    while (y < height) {
        if (input[y][x % width] == '#') {
            trees++
        }
        x += right
        y += down
    }

    trees
}

def result = countTrees(3, 1)
println result