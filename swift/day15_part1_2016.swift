
import Foundation

struct Disc {
    var totalPositions: Int
    var startPosition: Int
}

let fileURL = URL(fileURLWithPath: "input.txt")
let fileContent = try String(contentsOf: fileURL)
let lines = fileContent.components(separatedBy: .newlines)

var discs: [Disc] = []
let discRegex = try NSRegularExpression(pattern: "Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+).")

for line in lines {
    let matches = discRegex.matches(in: line, range: NSRange(line.startIndex..., in: line))
    let totalPositions = Int((line as NSString).substring(with: matches[0].range(at: 2)))!
    let startPosition = Int((line as NSString).substring(with: matches[0].range(at: 3)))!
    discs.append(Disc(totalPositions: totalPositions, startPosition: startPosition))
}

var time = 0
while true {
    if checkDiscs(discs: discs, time: time) {
        print(time)
        break
    }
    time += 1
}

func checkDiscs(discs: [Disc], time: Int) -> Bool {
    for (index, disc) in discs.enumerated() {
        let position = (disc.startPosition + time + index + 1) % disc.totalPositions
        if position != 0 {
            return false
        }
    }
    return true
}
