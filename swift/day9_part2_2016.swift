
import Foundation

func decompressedLength(_ input: String, recursive: Bool) -> Int {
    var length = 0
    var index = 0
    
    while index < input.count {
        if let markerRange = input.range(of: #"\(\d+x\d+\)"#, options: .regularExpression, range: input.index(input.startIndex, offsetBy: index)..<input.endIndex) {
            let marker = input[markerRange]
            let components = marker.replacingOccurrences(of: "(", with: "").replacingOccurrences(of: ")", with: "").components(separatedBy: "x")
            let repeatLength = Int(components[0])!
            let repeatCount = Int(components[1])!
            
            let markerStartIndex = input.index(markerRange.upperBound, offsetBy: 0)
            let markerEndIndex = input.index(markerStartIndex, offsetBy: repeatLength)
            let repeatedString = String(input[markerStartIndex..<markerEndIndex])
            
            if recursive {
                length += repeatCount * decompressedLength(repeatedString, recursive: true)
            } else {
                length += repeatCount * repeatLength
            }
            
            index = input.distance(from: input.startIndex, to: markerEndIndex)
        } else {
            length += 1
            index += 1
        }
    }
    
    return length
}

if let input = try? String(contentsOfFile: "input.txt") {
    let part1 = decompressedLength(input.replacingOccurrences(of: #"\s+"#, with: "", options: .regularExpression), recursive: false)
    let part2 = decompressedLength(input.replacingOccurrences(of: #"\s+"#, with: "", options: .regularExpression), recursive: true)
    
    print(part1)
    print(part2)
}
