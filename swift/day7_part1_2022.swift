
import Foundation

class File {
    var size: Int
    
    init(size: Int) {
        self.size = size
    }
}

class Directory {
    var files: [String: File]
    var directories: [String: Directory]
    
    init() {
        self.files = [:]
        self.directories = [:]
    }
    
    func totalSize() -> Int {
        var size = 0
        for (_, f) in files {
            size += f.size
        }
        for (_, dir) in directories {
            size += dir.totalSize()
        }
        return size
    }
}

let fileURL = URL(fileURLWithPath: "input.txt")
let fileContent = try String(contentsOf: fileURL, encoding: .utf8)
let lines = fileContent.components(separatedBy: .newlines)

let root = Directory()
var currentDir = root
var directoryStack = [root]

for line in lines {
    if line.hasPrefix("$ cd") {
        let path = line.replacingOccurrences(of: "$ cd", with: "").trimmingCharacters(in: .whitespaces)
        if path == "/" {
            currentDir = root
            directoryStack = [root]
        } else if path == ".." {
            directoryStack.removeLast()
            currentDir = directoryStack.last!
        } else {
            if currentDir.directories[path] == nil {
                currentDir.directories[path] = Directory()
            }
            currentDir = currentDir.directories[path]!
            directoryStack.append(currentDir)
        }
    } else if line.hasPrefix("dir") {
        let dirName = line.replacingOccurrences(of: "dir", with: "").trimmingCharacters(in: .whitespaces)
        currentDir.directories[dirName] = Directory()
    } else {
        let parts = line.components(separatedBy: " ")
        if parts.count == 2 {
            if let size = Int(parts[0]), let fileName = parts[safe: 1] {
                currentDir.files[fileName] = File(size: size)
            }
        }
    }
}

var sumSizes = 0
func calculateSizes(_ d: Directory) {
    let dirSize = d.totalSize()
    if dirSize <= 100000 {
        sumSizes += dirSize
    }
    for (_, dir) in d.directories {
        calculateSizes(dir)
    }
}
calculateSizes(root)

print(sumSizes)

extension Array {
    subscript(safe index: Index) -> Element? {
        return indices.contains(index) ? self[index] : nil
    }
}
