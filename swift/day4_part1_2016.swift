
import Foundation

let input = try String(contentsOfFile: "input.txt")
let rooms = input.components(separatedBy: "\n")

var sum = 0

for room in rooms {
    let components = room.components(separatedBy: "-")
    let name = components.dropLast(1).joined()
    let sectorAndChecksum = components.last!.components(separatedBy: "[")
    let sectorID = Int(sectorAndChecksum[0])!
    let checksum = sectorAndChecksum[1].replacingOccurrences(of: "]", with: "")

    var charFrequency: [Character: Int] = [:]
    for char in name {
        if char != "-" {
            charFrequency[char, default: 0] += 1
        }
    }

    let sortedFrequency = charFrequency.sorted {
        if $0.value == $1.value {
            return $0.key < $1.key
        }
        return $0.value > $1.value
    }

    let calculatedChecksum = String(sortedFrequency.prefix(5).map { $0.key })

    if calculatedChecksum == checksum {
        sum += sectorID
    }
}

print(sum)
