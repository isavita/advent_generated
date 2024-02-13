
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)

func dragonCurve(_ input: String, length: Int) -> String {
    var data = input

    while data.count < length {
        let a = data
        let b = String(a.reversed().map { $0 == "0" ? "1" : "0" })
        data = a + "0" + b
    }

    return String(data.prefix(length))
}

func checksum(_ data: String) -> String {
    var checksum = data

    while checksum.count % 2 == 0 {
        var newChecksum = ""

        for i in stride(from: 0, to: checksum.count, by: 2) {
            let pair = String(checksum[checksum.index(checksum.startIndex, offsetBy: i)..<checksum.index(checksum.startIndex, offsetBy: i+2)])
            newChecksum.append(pair == "00" || pair == "11" ? "1" : "0")
        }

        checksum = newChecksum
    }

    return checksum
}

let diskLength = 272
let result = checksum(dragonCurve(input, length: diskLength))
print(result)
