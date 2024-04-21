import Foundation

func filterValues(_ values: [String], criteria: (Int, Int) -> Character) -> String {
    var values = values
    for i in 0..<values[0].count {
        var zeros = 0, ones = 0
        for val in values {
            val[val.index(val.startIndex, offsetBy: i)] == "0" ? (zeros += 1) : (ones += 1)
        }
        let keep = criteria(zeros, ones)
        values = filterByBit(values, bitIndex: i, keep: keep)
        if values.count == 1 {
            break
        }
    }
    return values[0]
}

func filterByBit(_ values: [String], bitIndex: Int, keep: Character) -> [String] {
    return values.filter { $0[$0.index($0.startIndex, offsetBy: bitIndex)] == keep }
}

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let values = fileContent.components(separatedBy: "\n")
        
        let oxygenGeneratorRating = filterValues(values, criteria: { zeros, ones in zeros > ones ? "0" : "1" })
        let oxygenGeneratorRatingInt = Int(oxygenGeneratorRating, radix: 2)!
        
        let co2ScrubberRating = filterValues(values, criteria: { zeros, ones in zeros <= ones ? "0" : "1" })
        let co2ScrubberRatingInt = Int(co2ScrubberRating, radix: 2)!
        
        print(oxygenGeneratorRatingInt * co2ScrubberRatingInt)
    } catch {
        print("Error reading file")
    }
}

main()