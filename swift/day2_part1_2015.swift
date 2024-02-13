
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
if let input = try? String(contentsOf: fileURL) {
    var total = 0
    let lines = input.components(separatedBy: .newlines)
    for line in lines {
        let dimensions = line.components(separatedBy: "x")
        if dimensions.count != 3 {
            fatalError("Invalid input format")
        }
        
        if let l = Int(dimensions[0]), let w = Int(dimensions[1]), let h = Int(dimensions[2]) {
            let side1 = l * w
            let side2 = w * h
            let side3 = h * l
            
            let smallest = min(side1, side2, side3)
            total += 2*side1 + 2*side2 + 2*side3 + smallest
        }
    }
    
    print(total)
}

func min(_ vals: Int...) -> Int {
    var minVal = vals[0]
    for val in vals[1...] {
        if val < minVal {
            minVal = val
        }
    }
    return minVal
}
