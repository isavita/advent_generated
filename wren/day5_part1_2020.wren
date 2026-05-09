import "io" for File

var maxSeatID = 0
var content = File.read("input.txt")
var lines = content.split("\n")
for (line in lines) {
    if (line == "") continue
    var row = 0
    var col = 0
    for (i in 0..6) {
        row = row * 2
        if (line[i] == "B") row = row + 1
    }
    for (i in 7..9) {
        col = col * 2
        if (line[i] == "R") col = col + 1
    }
    var seatID = row * 8 + col
    if (seatID > maxSeatID) maxSeatID = seatID
}
System.print(maxSeatID)