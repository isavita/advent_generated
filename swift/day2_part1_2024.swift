
import Foundation

func solve() throws {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try String(contentsOf: fileURL)
    let lines = content.split(separator: "\n")
    var safeReportCount = 0

    for line in lines {
        let levels = line.split(separator: " ").compactMap { Int($0) }
        
        guard levels.count >= 2 else { continue }
        
        let firstDiff = levels[1] - levels[0]
        guard firstDiff != 0 else { continue }
        
        let isIncreasing = firstDiff > 0
        var isValid = true
        
        for i in 0..<levels.count - 1 {
            let diff = levels[i+1] - levels[i]
            
            guard diff != 0 else {
                isValid = false
                break
            }
            
            if (isIncreasing && diff <= 0) || (!isIncreasing && diff >= 0) {
                isValid = false
                break
            }
            
            let absDiff = abs(diff)
            guard absDiff >= 1 && absDiff <= 3 else {
                isValid = false
                break
            }
        }
        
        if isValid {
            safeReportCount += 1
        }
    }

    print(safeReportCount)
}

try solve()
