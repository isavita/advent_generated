
import Foundation

// ---------- Graph storage ----------
var nameToIdx = [String: Int]()
var adj = [[Int]]()                     // adjacency list

func idx(_ name: String) -> Int {
    if let i = nameToIdx[name] { return i }
    let i = adj.count
    nameToIdx[name] = i
    adj.append([])
    return i
}

func addEdge(_ u: Int, _ v: Int) {
    adj[u].append(v)
}

// ---------- Input ----------
let inputURL = URL(fileURLWithPath: "input.txt")
guard let data = try? Data(contentsOf: inputURL),
      let content = String(data: data, encoding: .utf8) else {
    exit(1)
}

for rawLine in content.split(separator: "\n", omittingEmptySubsequences: false) {
    var line = String(rawLine)
    // trim leading/trailing whitespace
    line = line.trimmingCharacters(in: .whitespacesAndNewlines)
    if line.isEmpty { continue }

    guard let colonPos = line.firstIndex(of: ":") else { continue }
    let src = String(line[..<colonPos]).trimmingCharacters(in: .whitespaces)
    let dstPart = line[line.index(after: colonPos)...]
    let u = idx(src)

    for token in dstPart.split(whereSeparator: { $0 == " " || $0 == "\t" }) {
        let v = idx(String(token))
        addEdge(u, v)
    }
}

// ---------- Ensure special nodes exist ----------
let svr = idx("svr")
let dac = idx("dac")
let fft = idx("fft")
let out = idx("out")

// ---------- DFS with memo ----------
func dfs(_ cur: Int, _ target: Int, _ memo: inout [Int64?]) -> Int64 {
    if cur == target { return 1 }
    if let cached = memo[cur] { return cached }

    var sum: Int64 = 0
    for nxt in adj[cur] {
        sum += dfs(nxt, target, &memo)
    }
    memo[cur] = sum
    return sum
}

func countPaths(_ s: Int, _ t: Int) -> Int64 {
    var memo = [Int64?](repeating: nil, count: adj.count)
    return dfs(s, t, &memo)
}

// ---------- Compute result ----------
let s1 = countPaths(svr, dac) * countPaths(dac, fft) * countPaths(fft, out)
let s2 = countPaths(svr, fft) * countPaths(fft, dac) * countPaths(dac, out)
let total = s1 + s2

print("Paths (svr->dac->fft->out): \(s1)")
print("Paths (svr->fft->dac->out): \(s2)")
print("Total paths visiting both: \(total)")
