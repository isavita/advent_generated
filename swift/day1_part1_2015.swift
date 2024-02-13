
import Foundation

do {
    let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
    var floor = 0
    for char in input {
        if char == "(" {
            floor += 1
        } else if char == ")" {
            floor -= 1
        }
    }
    print(floor)
} catch {
    print("Error reading the file: \(error)")
}
