
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
var x = [1]

input.enumerateLines { line, _ in
    switch line {
    case "noop":
        x.append(x.last!)
    default:
        let n = Int(line.components(separatedBy: " ")[1])!
        x.append(x.last!)
        x.append(x.last! + n)
    }
}

var sum = 0
for (index, value) in x.enumerated() {
    if (index - 19) % 40 == 0 {
        sum += (index + 1) * value
    }
}
print(sum)
