
import Foundation

func getDecompressedLength(input: String) -> Int {
    let markerRegex = try! NSRegularExpression(pattern: "\\((\\d+)x(\\d+)\\)")
    var length = 0
    var i = 0
    let nsInput = input as NSString
    while i < nsInput.length {
        if let marker = markerRegex.firstMatch(in: input, range: NSRange(location: i, length: nsInput.length - i)) {
            let charCountRange = marker.range(at: 1)
            let repeatCountRange = marker.range(at: 2)
            let charCount = Int(nsInput.substring(with: charCountRange))!
            let repeatCount = Int(nsInput.substring(with: repeatCountRange))!
            length += charCount * repeatCount
            i += marker.range.length + charCount
        } else {
            length += 1
            i += 1
        }
    }
    return length
}

guard let input = try? String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines) else {
    fatalError("Could not read input file")
}

let decompressedLength = getDecompressedLength(input: input)
print(decompressedLength)
