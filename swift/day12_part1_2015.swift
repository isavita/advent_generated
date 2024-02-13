import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let data = try Data(contentsOf: fileURL)
let jsonData = try JSONSerialization.jsonObject(with: data, options: [])

func sumNumbers(data: Any) -> Int {
    var sum = 0
    if let value = data as? [Any] {
        for v in value {
            sum += sumNumbers(data: v)
        }
    } else if let value = data as? [String: Any] {
        for (_, v) in value {
            sum += sumNumbers(data: v)
        }
    } else if let value = data as? Double {
        sum += Int(value)
    }
    return sum
}

let sum = sumNumbers(data: jsonData)
print(sum)