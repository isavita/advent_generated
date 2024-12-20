
import Foundation

func solve() {
    guard let data = try? String(contentsOfFile: "input.txt") else { return }
    let line = data.trimmingCharacters(in: .whitespacesAndNewlines)

    var disk: [Int?] = []
    var fileID = 0
    var isFile = true
    for char in line {
        guard let length = Int(String(char)) else { return }
        if isFile {
            disk.append(contentsOf: Array(repeating: fileID, count: length))
            fileID += 1
        } else {
            disk.append(contentsOf: Array(repeating: nil, count: length))
        }
        isFile = !isFile
    }

    while true {
        guard let lfree = disk.firstIndex(where: { $0 == nil }) else { break }
        guard let rfile = disk.lastIndex(where: { $0 != nil && $0! > -1 }) else { break }
        if rfile <= lfree { break }
        disk[lfree] = disk[rfile]
        disk[rfile] = nil
    }

    var checksum = 0
    for (i, val) in disk.enumerated() {
        if let id = val {
            checksum += i * id
        }
    }
    print(checksum)
}

solve()
