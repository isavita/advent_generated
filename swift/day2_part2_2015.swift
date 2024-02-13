import Foundation

let input = try String(contentsOfFile: "./input.txt")
let lines = input.components(separatedBy: .newlines)

var totalPaper = 0
var totalRibbon = 0

for line in lines {
    let dimensions = line.components(separatedBy: "x").compactMap { Int($0) }
    let l = dimensions[0]
    let w = dimensions[1]
    let h = dimensions[2]
    
    let side1 = l * w
    let side2 = w * h
    let side3 = h * l
    
    let smallestSide = min(side1, side2, side3)
    
    let paper = 2 * side1 + 2 * side2 + 2 * side3 + smallestSide
    totalPaper += paper
    
    let ribbon = 2 * (l + w + h - max(l, w, h)) + l * w * h
    totalRibbon += ribbon
}

print(totalPaper)
print(totalRibbon)
