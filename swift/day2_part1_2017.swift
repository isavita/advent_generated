
import Foundation

func calculateChecksum(fromFile filePath: String) {
    guard let fileContents = try? String(contentsOfFile: filePath) else {
        print("Error reading file.")
        return
    }

    let rows = fileContents.components(separatedBy: "\n").filter { !$0.isEmpty }
    var checksum = 0

    for row in rows {
        let numbers = row.components(separatedBy: .whitespaces)
                         .compactMap { Int($0) }
        
        if let min = numbers.min(), let max = numbers.max() {
            checksum += max - min
        }
    }

    print(checksum)
}

calculateChecksum(fromFile: "input.txt")
