import Foundation

func sumNumbers(_ data: Any) -> Int {
    var sum = 0
    if let array = data as? [Any] {
        for v in array {
            sum += sumNumbers(v)
        }
    } else if let dict = data as? [String: Any] {
        if !containsRed(dict) {
            for v in dict.values {
                sum += sumNumbers(v)
            }
        }
    } else if let num = data as? Int {
        sum += num
    }
    return sum
}

func containsRed(_ obj: [String: Any]) -> Bool {
    for v in obj.values {
        if let str = v as? String, str == "red" {
            return true
        }
    }
    return false
}

do {
    let data = try String(contentsOfFile: "input.txt")
    if let jsonData = try? JSONSerialization.jsonObject(with: data.data(using: .utf8)!) {
        let sum = sumNumbers(jsonData)
        print(sum)
    } else {
        print("Error parsing JSON")
    }
} catch {
    print("Error reading input: \(error)")
}