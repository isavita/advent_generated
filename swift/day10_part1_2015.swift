
import Foundation

guard let input = try? String(contentsOfFile: "input.txt") else { fatalError("Failed to load input") }

func lookAndSay(_ input: String) -> String {
    var result = ""
    var count = 1
    let chars = Array(input)
    
    for i in 0..<chars.count {
        if i < chars.count - 1 && chars[i] == chars[i + 1] {
            count += 1
        } else {
            result += "\(count)\(chars[i])"
            count = 1
        }
    }
    
    return result
}

var current = input
for _ in 1...40 {
    current = lookAndSay(current)
}

print(current.count)
