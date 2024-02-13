
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).components(separatedBy: .newlines)

let buses = input[1].components(separatedBy: ",").enumerated().compactMap { $0.element == "x" ? nil : (Int($0.element)!, $0.offset) }

var time = 0
var step = 1

for (busID, offset) in buses {
    while (time + offset) % busID != 0 {
        time += step
    }
    step *= busID
}

print(time)
