
import Foundation

struct MappingRange {
    let destinationStart: Int
    let sourceStart: Int
    let length: Int

    func mapSourceToDestination(_ source: Int) -> Int? {
        if source >= sourceStart && source < sourceStart + length {
            return destinationStart + (source - sourceStart)
        }
        return nil
    }
}

struct Map {
    let ranges: [MappingRange]

    func mapSourceToDestination(_ source: Int) -> Int {
        for range in ranges {
            if let destination = range.mapSourceToDestination(source) {
                return destination
            }
        }
        return source // If not found in any range, source maps to itself
    }
}

func parseInput(from fileURL: URL) -> ([Int], [Map])? {
    do {
        let input = try String(contentsOf: fileURL, encoding: .utf8)
        let sections = input.components(separatedBy: "\n\n")

        guard sections.count >= 8 else { return nil } // Ensure we have seeds and all maps.

        // Parse seeds
        let seedsString = sections[0]
        guard let seedsRange = seedsString.range(of: "seeds: ") else { return nil }
        let seeds = seedsString[seedsRange.upperBound...]
            .split(separator: " ")
            .compactMap { Int($0) }

        // Parse maps
        var maps: [Map] = []
        for section in sections.dropFirst() {
            let lines = section.components(separatedBy: .newlines).filter { !$0.isEmpty }
            var ranges: [MappingRange] = []
            for line in lines.dropFirst() { // Drop the title line
                let values = line.split(separator: " ").compactMap { Int($0) }
                guard values.count == 3 else { continue } //each line need exactly 3 values.
                ranges.append(MappingRange(destinationStart: values[0], sourceStart: values[1], length: values[2]))
            }
            maps.append(Map(ranges: ranges))
        }
        
        return (seeds, maps)

    } catch {
        print("Error reading file: \(error)")
        return nil
    }
}

func solve() {
    let fileURL = URL(fileURLWithPath: "input.txt")
    guard let (seeds, maps) = parseInput(from: fileURL) else {
        print("Failed to parse input.")
        return
    }

    var lowestLocation = Int.max

    for seed in seeds {
        var currentNumber = seed
        for map in maps {
            currentNumber = map.mapSourceToDestination(currentNumber)
        }
        lowestLocation = min(lowestLocation, currentNumber)
    }

    print(lowestLocation)
}

// Main entry point
solve()

