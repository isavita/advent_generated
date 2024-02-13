
import Foundation

// Read input from file
let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let numbers = input.components(separatedBy: " ").compactMap { Int($0) }

// Parse the tree and calculate the metadata sum
let result = parseTree(data: numbers, index: 0)
print(result.sum)

func parseTree(data: [Int], index: Int) -> (sum: Int, newIndex: Int) {
    var index = index
    let childCount = data[index]
    let metaCount = data[index + 1]
    index += 2

    var sum = 0
    for _ in 0..<childCount {
        let childResult = parseTree(data: data, index: index)
        sum += childResult.sum
        index = childResult.newIndex
    }

    for i in 0..<metaCount {
        sum += data[index + i]
    }
    index += metaCount

    return (sum, index)
}
