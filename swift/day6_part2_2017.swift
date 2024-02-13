
import Foundation

// Step 1: Read Input
let fileURL = URL(fileURLWithPath: "input.txt")
let data = try String(contentsOf: fileURL)
let strArr = data.components(separatedBy: .whitespacesAndNewlines).filter { !$0.isEmpty }
var banks = strArr.compactMap { Int($0) }

// Step 2: Initialize Variables
var seen: [String: Int] = [:]
var cycles = 0

// Step 3: Redistribution Loop
while true {
    // Convert current banks state to string to store in dictionary
    let state = "\(banks)"

    // Step 4: Check for Repeats
    if let prevCycle = seen[state] {
        print("The size of the loop is", cycles - prevCycle)
        break
    }
    seen[state] = cycles

    // Find the bank with most blocks
    var maxIndex = 0
    for i in 1..<banks.count {
        if banks[i] > banks[maxIndex] {
            maxIndex = i
        }
    }

    // Perform redistribution
    let blocks = banks[maxIndex]
    banks[maxIndex] = 0
    for i in 1...blocks {
        banks[(maxIndex + i) % banks.count] += 1
    }

    // Increment cycle counter
    cycles += 1
}
