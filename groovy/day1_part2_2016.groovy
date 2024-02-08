def dirs = [[0, 1], [1, 0], [0, -1], [-1, 0]]
def dir = 0
def x = 0
def y = 0
def visited = [[0, 0]]
def firstTwice = null

def lines = new File("input.txt").readLines()
def instr = lines[0].split(", ")

instr.each {
    def turn = it[0]
    def dist = it[1..-1] as int

    if (turn == 'R') dir = (dir + 1) % 4
    else dir = (dir + 3) % 4

    while (dist-- > 0) {
        x += dirs[dir][0]
        y += dirs[dir][1]

        if (firstTwice == null && [x, y] in visited) firstTwice = [x, y]
        visited << [x, y]
    }
}

println Math.abs(x) + Math.abs(y)
println Math.abs(firstTwice[0]) + Math.abs(firstTwice[1])