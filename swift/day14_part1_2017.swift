
import Foundation

// Function to calculate the knot hash
func knotHash(_ input: String) -> String {
    var lengths = input.unicodeScalars.map { Int($0.value) }
    lengths += [17, 31, 73, 47, 23]
    var list = Array(0..<256)
    var currentPosition = 0
    var skipSize = 0

    for _ in 0..<64 {
        for length in lengths {
            var i = currentPosition
            var j = (currentPosition + length - 1) % list.count
            for _ in 0..<length / 2 {
                list.swapAt(i, j)
                i = (i + 1) % list.count
                j = (j - 1 + list.count) % list.count
            }
            currentPosition = (currentPosition + length + skipSize) % list.count
            skipSize += 1
        }
    }

    var denseHash = ""
    for i in 0..<16 {
        var xor = 0
        for j in 0..<16 {
            xor ^= list[i * 16 + j]
        }
        denseHash += String(format: "%02x", xor)
    }
    return denseHash
}

// Function to convert hex to binary
func hexToBinary(_ hex: String) -> String {
    var binary = ""
    for char in hex {
        guard let intValue = Int(String(char), radix: 16) else { return "" }
        binary += String(intValue, radix: 2).padding(toLength: 4, withPad: "0", startingAt: 0)
    }
    return binary
}

// Read input from file
guard let input = try? String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines) else {
    fatalError("Could not read input file")
}

// Calculate used squares
var usedSquares = 0
for i in 0..<128 {
    let hashInput = "\(input)-\(i)"
    let hash = knotHash(hashInput)
    let binary = hexToBinary(hash)
    usedSquares += binary.filter { $0 == "1" }.count
}

// Print the result
print(usedSquares)
