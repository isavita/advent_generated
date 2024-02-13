
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

var digits = input.compactMap { Int(String($0)) }

func applyFFT(input: [Int]) -> [Int] {
    let basePattern = [0, 1, 0, -1]
    var output = [Int](repeating: 0, count: input.count)
    for i in 0..<input.count {
        var sum = 0
        for j in 0..<input.count {
            let patternValue = basePattern[((j+1)/(i+1)) % basePattern.count]
            sum += input[j] * patternValue
        }
        output[i] = abs(sum % 10)
    }
    return output
}

func abs(_ x: Int) -> Int {
    return x < 0 ? -x : x
}

for _ in 0..<100 {
    digits = applyFFT(input: digits)
}

for i in 0..<8 {
    print(digits[i], terminator: "")
}
print()
