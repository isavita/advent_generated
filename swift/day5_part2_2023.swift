
import Foundation

struct Range {
    let srcStart: Int
    let destStart: Int
    let length: Int
}

func reverseConvertNumber(number: Int, ranges: [Range]) -> Int {
    for r in ranges.reversed() {
        if r.destStart <= number && number < r.destStart + r.length {
            return r.srcStart + (number - r.destStart)
        }
    }
    return number
}

func isInSeedRanges(number: Int, seedRanges: [(Int, Int)]) -> Bool {
    for (start, length) in seedRanges {
        if start <= number && number < start + length {
            return true
        }
    }
    return false
}

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        return
    }

    let lines = input.components(separatedBy: .newlines)
    var seedRanges: [(Int, Int)] = []
    var maps: [[[String: Int]]] = []
    var currentRanges: [[String: Int]] = []

    for line in lines {
        let trimmedLine = line.trimmingCharacters(in: .whitespaces)
        if trimmedLine.contains("map:") {
            if !currentRanges.isEmpty {
                maps.append(currentRanges)
                currentRanges = []
            }
        } else if trimmedLine.hasPrefix("seeds:") {
            let seedInfo = trimmedLine.dropFirst(7).components(separatedBy: .whitespaces)
            for i in stride(from: 0, to: seedInfo.count, by: 2) {
                if let start = Int(seedInfo[i]), let length = Int(seedInfo[i + 1]) {
                    seedRanges.append((start, length))
                }
            }
        } else {
            let numbers = trimmedLine.components(separatedBy: .whitespaces)
            if numbers.count == 3 {
                if let destStart = Int(numbers[0]),
                   let srcStart = Int(numbers[1]),
                   let length = Int(numbers[2]) {
                    currentRanges.append(["srcStart": srcStart, "destStart": destStart, "length": length])
                }
            }
        }
    }
    if !currentRanges.isEmpty {
        maps.append(currentRanges)
    }
    
    let swiftMaps = maps.map { map in
        map.map { rangeDict -> Range in
            return Range(srcStart: rangeDict["srcStart"]!, destStart: rangeDict["destStart"]!, length: rangeDict["length"]!)
        }
    }

    var location = 0
    while true {
        var seed = location
        for m in swiftMaps.reversed() {
            seed = reverseConvertNumber(number: seed, ranges: m)
        }

        if isInSeedRanges(number: seed, seedRanges: seedRanges) {
            print(location)
            break
        }
        location += 1
    }
}

main()
