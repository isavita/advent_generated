
import Foundation

let input = try String(contentsOfFile: "input.txt")
let boardingPasses = input.components(separatedBy: .newlines)

var maxSeatID = 0

for boardingPass in boardingPasses {
    var rowRange = 0...127
    var colRange = 0...7
    
    for char in boardingPass {
        if char == "F" {
            rowRange = rowRange.lowerBound...(rowRange.lowerBound + rowRange.count/2 - 1)
        } else if char == "B" {
            rowRange = (rowRange.lowerBound + rowRange.count/2)...rowRange.upperBound
        } else if char == "L" {
            colRange = colRange.lowerBound...(colRange.lowerBound + colRange.count/2 - 1)
        } else if char == "R" {
            colRange = (colRange.lowerBound + colRange.count/2)...colRange.upperBound
        }
    }
    
    let seatID = rowRange.lowerBound * 8 + colRange.lowerBound
    maxSeatID = max(maxSeatID, seatID)
}

print(maxSeatID)
