
import Foundation

extension String {
    func chunked(into size: Int) -> [String] {
        return stride(from: 0, to: count, by: size).map {
            let startIndex = index(startIndex, offsetBy: $0)
            let endIndex = index(startIndex, offsetBy: size, limitedBy: endIndex) ?? endIndex
            return String(self[startIndex..<endIndex])
        }
    }
}

let input = try! String(contentsOfFile: "input.txt")
let width = 25
let height = 6
let layerSize = width * height
let layers = input.chunked(into: layerSize)

var minZeroCount = Int.max
var result = 0

for layer in layers {
    let zeroCount = layer.filter { $0 == "0" }.count
    if zeroCount < minZeroCount {
        minZeroCount = zeroCount
        let ones = layer.filter { $0 == "1" }.count
        let twos = layer.filter { $0 == "2" }.count
        result = ones * twos
    }
}

print(result)
