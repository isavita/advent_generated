
import strutils

var maxSeatID = 0

for line in open("input.txt").lines:
    var rowLow = 0
    var rowHigh = 127
    var colLow = 0
    var colHigh = 7

    for c in line:
        if c == 'F':
            rowHigh = (rowLow + rowHigh) div 2
        elif c == 'B':
            rowLow = (rowLow + rowHigh + 1) div 2
        elif c == 'L':
            colHigh = (colLow + colHigh) div 2
        elif c == 'R':
            colLow = (colLow + colHigh + 1) div 2

    let seatID = rowLow * 8 + colLow
    if seatID > maxSeatID:
        maxSeatID = seatID

echo maxSeatID
