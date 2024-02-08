def input = new File("input.txt").text.trim().tokenize(", ")

def x = 0
def y = 0
def dir = 0

input.each { instr ->
    def turn = instr[0]
    def dist = instr[1..-1].toInteger()
    
    dir = (dir + (turn == 'R' ? 1 : -1) + 4) % 4
    
    if (dir == 0) y += dist
    else if (dir == 1) x += dist
    else if (dir == 2) y -= dist
    else x -= dist
}

println Math.abs(x) + Math.abs(y)