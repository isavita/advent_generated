
import Foundation

func readInput(filename: String) -> (String, [[Character]]) {
    let fileURL = URL(fileURLWithPath: filename)
    guard let content = try? String(contentsOf: fileURL) else { fatalError() }
    let lines = content.components(separatedBy: "\n").filter { !$0.isEmpty }
    let algorithm = lines[0]
    let image = lines.dropFirst().map { Array($0) }
    return (algorithm, image)
}

func enhanceImage(image: [[Character]], algorithm: String, times: Int) -> [[Character]] {
    var image = image
    for i in 0..<times {
        image = applyAlgorithm(image: image, algorithm: algorithm, flip: i % 2 == 1 && algorithm.first == "#")
    }
    return image
}

func applyAlgorithm(image: [[Character]], algorithm: String, flip: Bool) -> [[Character]] {
    let algArray = Array(algorithm)
    var enhancedImage = [[Character]](repeating: [Character](repeating: ".", count: image[0].count + 2), count: image.count + 2)
    let offsets = (-1...1).flatMap { di in (-1...1).map { dj in (di, dj) } }

    for i in 0..<enhancedImage.count {
        for j in 0..<enhancedImage[0].count {
            var index = 0
            for (di, dj) in offsets {
                let row = i - 1 + di
                let col = j - 1 + dj
                index <<= 1
                if row >= 0 && row < image.count && col >= 0 && col < image[0].count {
                    if image[row][col] == "#" {
                        index |= 1
                    }
                } else if flip {
                    index |= 1
                }
            }
            enhancedImage[i][j] = algArray[index]
        }
    }
    return enhancedImage
}

func countLitPixels(image: [[Character]]) -> Int {
    image.flatMap { $0 }.filter { $0 == "#" }.count
}

let (algorithm, image) = readInput(filename: "input.txt")
let enhanced = enhanceImage(image: image, algorithm: algorithm, times: 2)
print(countLitPixels(image: enhanced))
