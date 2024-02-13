
import Foundation

if let data = try? String(contentsOfFile: "input.txt") {
    let steps = Int(data.trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    var currentPos = 0
    var valueAfterZero = 0
    
    for i in 1...50000000 {
        currentPos = (currentPos + steps) % i
        if currentPos == 0 {
            valueAfterZero = i
        }
        currentPos += 1
    }
    
    print(valueAfterZero)
}
