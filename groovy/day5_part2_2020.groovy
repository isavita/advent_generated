
def input = new File("input.txt").readLines()

def seatIds = input.collect { line ->
    def row = Integer.parseInt(line[0..6].replace('F', '0').replace('B', '1'), 2)
    def col = Integer.parseInt(line[7..9].replace('L', '0').replace('R', '1'), 2)
    row * 8 + col
}

def maxSeatId = seatIds.max()

def mySeatId = (0..maxSeatId).find { it -> !seatIds.contains(it) && seatIds.contains(it + 1) && seatIds.contains(it - 1) }

println maxSeatId
println mySeatId
