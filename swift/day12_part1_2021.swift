import Foundation

class Cave {
    var connections: [String: Bool] = [:]

    func connect(to: String) {
        connections[to] = true
    }

    func disconnect(from: String) {
        connections[from] = nil
    }
}

var caves: [String: Cave] = [:]

if let fileHandle = FileHandle(forReadingAtPath: "input.txt") {
    defer { fileHandle.closeFile() }
    var data = Data()
    while true {
        let tempData = fileHandle.readData(ofLength: 1024)
        if tempData.count == 0 {
            break
        }
        data.append(tempData)
    }
    let string = String(data: data, encoding: .utf8)!
    let lines = string.components(separatedBy: "\n")
    for line in lines {
        if !line.isEmpty {
            let paths = line.components(separatedBy: "-")
            let from = paths[0]
            let to = paths[1]

            if caves[from] == nil {
                caves[from] = Cave()
            }

            if caves[to] == nil {
                caves[to] = Cave()
            }

            caves[from]!.connect(to: to)
            caves[to]!.connect(to: from)
        }
    }
}

var count = 0
func dfs(_ current: String, _ visited: [String: Bool]) {
    if current == "end" {
        count += 1
        return
    }

    for (next, _) in caves[current]!.connections {
        if visited[next] ?? false, next.lowercased() == next {
            continue
        }

        var visitedCopy = visited
        visitedCopy[next] = true
        dfs(next, visitedCopy)
    }
}

dfs("start", ["start": true])
print(count)