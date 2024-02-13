
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)
let lengths = input.components(separatedBy: ",").compactMap { Int($0) }

var list = Array(0...255)
var currentPosition = 0
var skipSize = 0

for length in lengths {
    let endIndex = currentPosition + length
    if endIndex < list.count {
        let sublist = Array(list[currentPosition..<endIndex])
        list.replaceSubrange(currentPosition..<endIndex, with: sublist.reversed())
    } else {
        let overflow = endIndex % list.count
        var sublist = Array(list[currentPosition...] + list[..<overflow])
        sublist.reverse()
        list.replaceSubrange(currentPosition..<list.count, with: sublist.prefix(list.count - currentPosition))
        list.replaceSubrange(0..<overflow, with: sublist.suffix(overflow))
    }
    
    currentPosition = (currentPosition + length + skipSize) % list.count
    skipSize += 1
}

print(list[0] * list[1])
