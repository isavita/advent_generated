
import Foundation

if let input = try? String(contentsOfFile: "input.txt") {
    if let target = Int(input.trimmingCharacters(in: .whitespacesAndNewlines)) {
        var target = target / 11
        var houses = [Int](repeating: 0, count: target + 1)
        
        for elf in 1...target {
            for house in stride(from: elf, through: min(elf*50, target), by: elf) {
                houses[house] += elf
            }
        }
        
        if let houseNumber = houses.firstIndex(where: { $0 >= target }) {
            print(houseNumber)
        }
    }
}
