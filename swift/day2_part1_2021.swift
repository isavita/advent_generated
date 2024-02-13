
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
if let content = try? String(contentsOf: fileURL) {
    var horizontalPosition = 0
    var depth = 0
    
    let lines = content.components(separatedBy: .newlines)
    
    for line in lines {
        let command = line.components(separatedBy: " ")
        let direction = command[0]
        let units = Int(command[1]) ?? 0
        
        switch direction {
        case "forward":
            horizontalPosition += units
        case "down":
            depth += units
        case "up":
            depth -= units
        default:
            break
        }
    }
    
    let product = horizontalPosition * depth
    print(product)
}
