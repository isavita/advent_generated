
import Foundation

func solve() {
    guard let raw = try? String(contentsOfFile: "input.txt", encoding: .utf8)
            .components(separatedBy: .newlines)
            .filter({ !$0.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty }) else {
        print(0)
        return
    }

    guard raw.count % 7 == 0 else {
        print(0)
        return
    }

    var locks: [[Int]] = []
    var keys: [[Int]] = []

    for i in stride(from: 0, to: raw.count, by: 7) {
        let block = Array(raw[i..<min(i + 7, raw.count)])
        if block.contains(where: { $0.count < 5 }) {
            continue
        }

        if block[0].allSatisfy({ $0 == "#" }) {
            locks.append(parseLock(block: block))
        } else {
            keys.append(parseKey(block: block))
        }
    }

    var count = 0
    for lock in locks {
        for key in keys {
            if fits(lock: lock, key: key) {
                count += 1
            }
        }
    }

    print(count)
}

func parseLock(block: [String]) -> [Int] {
    var h: [Int] = []
    for c in 0..<5 {
        var cnt = 0
        for r in 1..<7 {
            if block[r][String.Index(utf16Offset: c, in: block[r])] == "#" {
                cnt += 1
            } else {
                break
            }
        }
        h.append(cnt)
    }
    return h
}

func parseKey(block: [String]) -> [Int] {
    var h: [Int] = []
    for c in 0..<5 {
        var cnt = 0
        for r in (0..<6).reversed() {
            if block[r][String.Index(utf16Offset: c, in: block[r])] == "#" {
                cnt += 1
            } else {
                break
            }
        }
        h.append(cnt)
    }
    return h
}

func fits(lock: [Int], key: [Int]) -> Bool {
    for i in 0..<5 {
        if lock[i] + key[i] > 5 {
            return false
        }
    }
    return true
}

solve()
