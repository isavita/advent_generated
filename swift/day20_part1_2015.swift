import Foundation

if let input = try? String(contentsOfFile: "input.txt") {
    let targetValue = Int(input.trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    var target = targetValue / 10

    var houses = Array(repeating: 0, count: target + 1)

    for elf in 1...target {
        for house in stride(from: elf, through: target, by: elf) {
            houses[house] += elf
        }
    }

    for (houseNumber, presents) in houses.enumerated() {
        if presents >= target {
            print(houseNumber)
            break
        }
    }
}