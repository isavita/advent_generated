def input = new File("input.txt").text.tokenize("\n")

def calculateSeatId = { boardingPass ->
    def row = Integer.parseInt(boardingPass[0..6].replace('F', '0').replace('B', '1'), 2)
    def column = Integer.parseInt(boardingPass[7..9].replace('L', '0').replace('R', '1'), 2)
    row * 8 + column
}

def seatIds = input.collect { calculateSeatId(it) }

println seatIds.max()