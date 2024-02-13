
import Foundation

let input = try String(contentsOfFile: "input.txt")
let boardingPasses = input.components(separatedBy: "\n")

func calculateSeatID(_ boardingPass: String) -> Int {
    var rowRange = 0...127
    var columnRange = 0...7
    
    for char in boardingPass {
        if char == "F" {
            rowRange = rowRange.lowerBound...(rowRange.lowerBound + rowRange.count/2 - 1)
        } else if char == "B" {
            rowRange = (rowRange.lowerBound + rowRange.count/2)...rowRange.upperBound
        } else if char == "L" {
            columnRange = columnRange.lowerBound...(columnRange.lowerBound + columnRange.count/2 - 1)
        } else if char == "R" {
            columnRange = (columnRange.lowerBound + columnRange.count/2)...columnRange.upperBound
        }
    }
    
    return rowRange.lowerBound * 8 + columnRange.lowerBound
}

var seatIDs = Set<Int>()
var maxSeatID = 0

for boardingPass in boardingPasses {
    let seatID = calculateSeatID(boardingPass)
    seatIDs.insert(seatID)
    maxSeatID = max(maxSeatID, seatID)
}

print(maxSeatID)

for seatID in 0...(127*8+7) {
    if !seatIDs.contains(seatID) && seatIDs.contains(seatID-1) && seatIDs.contains(seatID+1) {
        print(seatID)
        break
    }
}
