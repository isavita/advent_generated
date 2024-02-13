import Foundation

extension Data {
    func toString() -> String {
        return String(data: self, encoding: .utf8) ?? ""
    }
}

let fileURL = URL(fileURLWithPath: "input.txt")
let steps = Int((try! String(contentsOf: fileURL)).trimmingCharacters(in: .whitespacesAndNewlines))!
var buffer = [0]
var currentPosition = 0

for i in 1...2017 {
    currentPosition = (currentPosition + steps) % buffer.count + 1
    buffer.insert(i, at: currentPosition)
}

print(buffer[(currentPosition + 1) % buffer.count])