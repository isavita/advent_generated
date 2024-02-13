
import Foundation

// Read input from a file
let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)

// Convert input to ASCII codes
var lengths = [Int]()
for char in input.utf8 {
    lengths.append(Int(char))
}
lengths += [17, 31, 73, 47, 23]

// Initialize variables
var list = Array(0...255)
var currentPosition = 0
var skipSize = 0

// Perform 64 rounds
for _ in 0..<64 {
    for length in lengths {
        for i in 0..<(length/2) {
            let start = (currentPosition + i) % 256
            let end = (currentPosition + length - 1 - i) % 256
            list.swapAt(start, end)
        }
        currentPosition = (currentPosition + length + skipSize) % 256
        skipSize += 1
    }
}

// Calculate the dense hash
var denseHash = [UInt8]()
for i in stride(from: 0, to: 256, by: 16) {
    var xor = 0
    for j in 0..<16 {
        xor ^= list[i+j]
    }
    denseHash.append(UInt8(xor))
}

// Convert to hexadecimal
let hexHash = denseHash.map { String(format: "%02x", $0) }.joined()

print(hexHash)
