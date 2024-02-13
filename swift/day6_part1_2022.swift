
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

func firstNUnique(_ s: String, _ n: Int) -> Int {
    for i in n..<s.count {
        let startIndex = s.index(s.startIndex, offsetBy: i-n)
        let endIndex = s.index(s.startIndex, offsetBy: i)
        let subString = String(s[startIndex..<endIndex])
        let uniqueChars = Set(subString)
        if subString.count == uniqueChars.count {
            return i
        }
    }
    return -1
}

print(firstNUnique(input, 4))
