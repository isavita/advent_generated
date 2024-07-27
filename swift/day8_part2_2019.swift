
import Foundation

let data = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let width = 25, height = 6
let layerSize = width * height
var finalImage = Array(repeating: Character("2"), count: layerSize)

for i in stride(from: 0, to: data.count, by: layerSize) {
    let layerEnd = min(i + layerSize, data.count)
    let layer = Array(data[data.index(data.startIndex, offsetBy: i)..<data.index(data.startIndex, offsetBy: layerEnd)])

    for (j, pixel) in layer.enumerated() {
        if finalImage[j] == "2" {
            finalImage[j] = pixel
        }
    }
}

print("Decoded image:")
for i in 0..<height {
    for j in 0..<width {
        let pixel = finalImage[i * width + j]
        print(pixel == "0" ? " " : "#", terminator: "")
    }
    print()
}
