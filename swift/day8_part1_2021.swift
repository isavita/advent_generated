import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
do {
    let content = try String(contentsOf: fileURL, encoding: .utf8)
    let lines = content.components(separatedBy: "\n")
    var count = 0
    for line in lines {
        let parts = line.components(separatedBy: " | ")
        let output = parts[1]
        let digits = output.components(separatedBy: " ")
        for digit in digits {
            switch digit.count {
            case 2, 4, 3, 7:
                count += 1
            default:
                break
            }
        }
    }
    print(count)
} catch {
    print("Error reading file")
}