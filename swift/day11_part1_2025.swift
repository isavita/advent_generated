import Foundation

let path = "input.txt"
guard let content = try? String(contentsOfFile: path) else { exit(0) }
var nameToIdx = [String:Int]()
var adj = [[Int]]()

func idx(of name: String) -> Int {
    if let i = nameToIdx[name] { return i }
    let i = adj.count
    nameToIdx[name] = i
    adj.append([])
    return i
}

for rawLine in content.split(separator: "\n", omittingEmptySubsequences: false) {
    let line = rawLine.trimmingCharacters(in: .whitespacesAndNewlines)
    guard let colon = line.firstIndex(of: ":") else { continue }
    let src = String(line[..<colon]).trimmingCharacters(in: .whitespaces)
    let rest = line[line.index(after: colon)...].trimmingCharacters(in: .whitespaces)
    guard !src.isEmpty && !rest.isEmpty else { continue }
    let u = idx(of: src)
    for token in rest.split(separator: " ") {
        let vName = String(token)
        let v = idx(of: vName)
        adj[u].append(v)
    }
}

let start = idx(of: "you")
let end = idx(of: "out")
var memo = Array(repeating: -1, count: adj.count)

func dfs(_ u: Int) -> Int {
    if u == end { return 1 }
    if memo[u] != -1 { return memo[u] }
    var total = 0
    for v in adj[u] {
        total += dfs(v)
    }
    memo[u] = total
    return total
}

print(dfs(start))