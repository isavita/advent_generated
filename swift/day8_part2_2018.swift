
import Foundation

// Read input from file
let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let numbers = input.components(separatedBy: " ").compactMap { Int($0) }

func parseTree(data: [Int], index: Int) -> (Int, Int) {
    var index = index
    let childCount = data[index]
    let metaCount = data[index + 1]
    index += 2

    var childValues = [Int]()
    for _ in 0..<childCount {
        let (childValue, newIndex) = parseTree(data: data, index: index)
        childValues.append(childValue)
        index = newIndex
    }

    var value = 0
    if childCount == 0 {
        for i in 0..<metaCount {
            value += data[index + i]
        }
    } else {
        for i in 0..<metaCount {
            let metadata = data[index + i]
            if metadata <= childCount && metadata > 0 {
                value += childValues[metadata - 1]
            }
        }
    }
    index += metaCount

    return (value, index)
}

let (value, _) = parseTree(data: numbers, index: 0)
print(value)
