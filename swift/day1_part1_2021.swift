
import Foundation

if let input = try? String(contentsOfFile: "input.txt") {
    let depths = input.components(separatedBy: "\n").compactMap { Int($0) }
    var count = 0
    
    for i in 1..<depths.count {
        if depths[i] > depths[i - 1] {
            count += 1
        }
    }
    
    print(count)
}
