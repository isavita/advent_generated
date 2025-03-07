
import Foundation

func dragonCurve(_ a: String) -> String {
    let b = a.reversed().map { $0 == "1" ? "0" : "1" }.joined()
    return a + "0" + b
}

func generateData(_ initialState: String, _ diskLength: Int) -> String {
    var data = initialState
    while data.count < diskLength {
        data = dragonCurve(data)
    }
    return String(data.prefix(diskLength))
}

func calculateChecksum(_ data: String) -> String {
    var checksum = ""
    var i = data.startIndex
    while i < data.endIndex {
        let next = data.index(after: i)
        if next < data.endIndex {
            checksum += data[i] == data[next] ? "1" : "0"
            i = data.index(after: next)
        } else {
            break
        }
    }
    return checksum.count % 2 == 0 ? calculateChecksum(checksum) : checksum
}

func solve(diskLength: Int) {
    guard let initialState = try? String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines) else {
        return
    }

    let diskData = generateData(initialState, diskLength)
    let checksum = calculateChecksum(diskData)
    print(checksum)
}

func main() {

    solve(diskLength: 272)
    solve(diskLength: 35651584)
}

main()
