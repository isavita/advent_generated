
import Foundation

// Function to calculate the fuel requirement for a given mass
func calculateFuel(for mass: Int) -> Int {
    return max(mass / 3 - 2, 0)
}

// Function to calculate the total fuel requirement including fuel for fuel
func calculateTotalFuel(for mass: Int) -> Int {
    var totalFuel = 0
    var currentMass = mass
    
    while currentMass > 0 {
        let fuel = calculateFuel(for: currentMass)
        totalFuel += fuel
        currentMass = fuel
    }
    
    return totalFuel
}

// Read input from the file
func readInput(from filename: String) -> [Int] {
    do {
        let contents = try String(contentsOfFile: filename)
        let masses = contents.split(separator: "\n").compactMap { Int($0) }
        return masses
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

// Main execution
let masses = readInput(from: "input.txt")

// Part 1: Calculate total fuel requirement
let totalFuelPart1 = masses.reduce(0) { $0 + calculateFuel(for: $1) }
print("Total fuel requirement (Part 1): \(totalFuelPart1)")

// Part 2: Calculate total fuel requirement including fuel for fuel
let totalFuelPart2 = masses.reduce(0) { $0 + calculateTotalFuel(for: $1) }
print("Total fuel requirement (Part 2): \(totalFuelPart2)")
