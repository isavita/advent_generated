
import Foundation

class FileSegment {
    let id: Int
    let start: Int
    let end: Int

    init(id: Int, start: Int, end: Int) {
        self.id = id
        self.start = start
        self.end = end
    }
}

func solve() {
    guard let input = try? String(contentsOfFile: "input.txt", encoding: .utf8).trimmingCharacters(in: .whitespacesAndNewlines) else {
        print("Error reading input file")
        return
    }

    var disk: [String] = []
    var fileId = 0
    var isFile = true

    for char in input {
        if let length = Int(String(char)) {
            if isFile {
                disk.append(contentsOf: Array(repeating: String(fileId), count: length))
                fileId += 1
            } else {
                disk.append(contentsOf: Array(repeating: ".", count: length))
            }
            isFile.toggle()
        }
    }

    var files: [FileSegment] = []
    var currId: Int? = nil
    var start = 0

    for (i, val) in disk.enumerated() {
        if val == "." {
            currId = nil
            continue
        }

        let fileId = Int(val)!
        if fileId != currId {
            currId = fileId
            start = i
        }

        if i == disk.count - 1 || (i + 1 < disk.count && disk[i + 1] != val) {
            files.append(FileSegment(id: fileId, start: start, end: i))
        }
    }

    for file in files.reversed() {
        let fileLen = file.end - file.start + 1
        var leftmostSpan = -1
        var spanLen = 0

        for i in 0..<file.start {
            if disk[i] == "." {
                if spanLen == 0 {
                    leftmostSpan = i
                }
                spanLen += 1
                if spanLen == fileLen {
                    break
                }
            } else {
                spanLen = 0
                leftmostSpan = -1
            }
        }

        if leftmostSpan != -1 && spanLen == fileLen {
            for i in file.start...file.end {
                disk[i] = "."
            }
            for i in 0..<fileLen {
                disk[leftmostSpan + i] = String(file.id)
            }
        }
    }

    var checksum = 0
    for (i, val) in disk.enumerated() {
        if val != ".", let intVal = Int(val) {
            checksum += i * intVal
        }
    }

    print(checksum)
}

solve()
