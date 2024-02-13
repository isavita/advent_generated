
import Foundation

// Read input from file
let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).components(separatedBy: .newlines)

// Function to calculate gamma rate
func calculateGammaRate(input: [String]) -> Int {
    var gammaRate = ""
    for i in 0..<input[0].count {
        var count0 = 0
        var count1 = 0
        for num in input {
            if num[num.index(num.startIndex, offsetBy: i)] == "0" {
                count0 += 1
            } else {
                count1 += 1
            }
        }
        gammaRate += count0 > count1 ? "0" : "1"
    }
    return Int(gammaRate, radix: 2) ?? 0
}

// Function to calculate epsilon rate
func calculateEpsilonRate(input: [String]) -> Int {
    var epsilonRate = ""
    for i in 0..<input[0].count {
        var count0 = 0
        var count1 = 0
        for num in input {
            if num[num.index(num.startIndex, offsetBy: i)] == "0" {
                count0 += 1
            } else {
                count1 += 1
            }
        }
        epsilonRate += count0 < count1 ? "0" : "1"
    }
    return Int(epsilonRate, radix: 2) ?? 0
}

// Calculate gamma rate and epsilon rate
let gammaRate = calculateGammaRate(input: input)
let epsilonRate = calculateEpsilonRate(input: input)

// Calculate power consumption
let powerConsumption = gammaRate * epsilonRate
print(powerConsumption)
