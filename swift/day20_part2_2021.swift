
import Foundation

let iterations = 50
let expandBy = 1

func readInput(_ filename: String) -> (String, [[Bool]]) {
    let fileURL = URL(fileURLWithPath: filename)
    let input = try! String(contentsOf: fileURL)
    let lines = input.components(separatedBy: .newlines)
    
    let algorithm = lines[0]
    var image = [[Bool]]()
    
    for i in 2..<lines.count {
        let line = lines[i].trimmingCharacters(in: .whitespaces)
        var row = [Bool]()
        for char in line {
            row.append(char == "#")
        }
        image.append(row)
    }
    
    return (algorithm, image)
}

func enhanceImage(_ algorithm: String, _ image: [[Bool]], _ useInfiniteLit: Bool) -> [[Bool]] {
    var newImage = [[Bool]](repeating: [Bool](repeating: false, count: image[0].count + (expandBy * 2)), count: image.count + (expandBy * 2))
    
    for y in -expandBy..<(image.count + expandBy) {
        for x in -expandBy..<(image[0].count + expandBy) {
            var index = 0
            for dy in -1...1 {
                for dx in -1...1 {
                    index <<= 1
                    let ny = y + dy
                    let nx = x + dx
                    if ny >= 0 && ny < image.count && nx >= 0 && nx < image[0].count {
                        if image[ny][nx] {
                            index |= 1
                        }
                    } else if useInfiniteLit {
                        index |= 1
                    }
                }
            }
            newImage[y + expandBy][x + expandBy] = algorithm[algorithm.index(algorithm.startIndex, offsetBy: index)] == "#"
        }
    }
    
    return newImage
}

func countLitPixels(_ image: [[Bool]]) -> Int {
    var count = 0
    for row in image {
        for pixel in row {
            if pixel {
                count += 1
            }
        }
    }
    return count
}

let (algorithm, image) = readInput("input.txt")
var currentImage = image

for i in 0..<iterations {
    currentImage = enhanceImage(algorithm, currentImage, i % 2 == 1 && algorithm.first == "#")
}

print(countLitPixels(currentImage))
