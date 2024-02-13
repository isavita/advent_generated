import Foundation

var root = [""]
var dirs = [String: Int]()
var files = [String: Int]()
var curr = [String]()

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

for line in lines {
    let txt = line.components(separatedBy: " ")
    if txt[0] == "$" {
        if txt[1] == "cd" {
            if txt[2] == "/" {
                curr = root
            } else if txt[2] == ".." {
                curr.removeLast()
            } else {
                curr.append(txt[2])
            }
            dirs[curr.joined(separator: "/")] = 0
        }
    } else {
        if txt[0] != "dir" {
            files[curr.joined(separator: "/") + "/" + txt[1]] = Int(txt[0]) ?? 0
        }
    }
}

for (f, s) in files {
    let path = f.components(separatedBy: "/")
    for i in 1..<path.count {
        dirs[path[0..<i].joined(separator: "/")]! += s
    }
}

var sortedSizes = [Int]()
for (_, s) in dirs {
    sortedSizes.append(s)
}

sortedSizes.sort()
let total = 70000000
let want = 30000000
let available = total - dirs[""]!
print(sortedSizes[sortedSizes.firstIndex(where: { $0 >= want - available })!])