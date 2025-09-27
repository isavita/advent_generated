import Foundation

// MARK: - Intcode emulator (step-wise ASCII)

enum EmuStatus { case halted, output, waiting }

final class Emulator {
    typealias IntT = Int64
    private var mem: [IntT: IntT] = [:]
    private var ip: IntT = 0
    private var rb: IntT = 0
    private var inputQ: [IntT] = []

    init(program: [IntT]) {
        for (i, v) in program.enumerated() { mem[IntT(i)] = v }
    }
    private func mget(_ a: IntT) -> IntT { mem[a] ?? 0 }
    private func mset(_ a: IntT, _ v: IntT) { mem[a] = v }
    private func rd(_ idx: IntT, _ m: IntT) -> IntT {
        let v = mget(ip + idx)
        switch m {
        case 0: return mget(v)
        case 1: return v
        case 2: return mget(rb + v)
        default: fatalError("bad mode")
        }
    }
    private func wa(_ idx: IntT, _ m: IntT) -> IntT {
        let v = mget(ip + idx)
        switch m {
        case 0: return v
        case 2: return rb + v
        default: fatalError("bad write mode")
        }
    }
    func writeString(_ s: String) {
        inputQ.append(contentsOf: s.utf8.map { IntT($0) })
    }
    /// Runs until: emits one output, needs input (and queue empty), or halts.
    func step() -> (IntT?, EmuStatus) {
        while true {
            let op = mget(ip)
            let oc = op % 100
            let m1 = (op / 100) % 10
            let m2 = (op / 1000) % 10
            let m3 = (op / 10000) % 10
            switch oc {
            case 1:
                let a = rd(1, m1), b = rd(2, m2), c = wa(3, m3)
                mset(c, a + b); ip += 4
            case 2:
                let a = rd(1, m1), b = rd(2, m2), c = wa(3, m3)
                mset(c, a * b); ip += 4
            case 3:
                let a = wa(1, m1)
                if inputQ.isEmpty { return (nil, .waiting) }
                mset(a, inputQ.removeFirst()); ip += 2
            case 4:
                let a = rd(1, m1); ip += 2
                return (a, .output)
            case 5:
                let a = rd(1, m1), b = rd(2, m2)
                ip = (a != 0) ? b : (ip + 3)
            case 6:
                let a = rd(1, m1), b = rd(2, m2)
                ip = (a == 0) ? b : (ip + 3)
            case 7:
                let a = rd(1, m1), b = rd(2, m2), c = wa(3, m3)
                mset(c, (a < b) ? 1 : 0); ip += 4
            case 8:
                let a = rd(1, m1), b = rd(2, m2), c = wa(3, m3)
                mset(c, (a == b) ? 1 : 0); ip += 4
            case 9:
                let a = rd(1, m1); rb += a; ip += 2
            case 99:
                return (nil, .halted)
            default:
                fatalError("bad opcode \(oc)")
            }
        }
    }
}

// MARK: - World / solver

struct Room {
    var name: String
    // dir -> room index (>=0) or -1 for unknown
    var connections: [String: Int]
    init(_ name: String) {
        self.name = name
        self.connections = [:]
    }
}

enum Mode { case explore, navigate, test }

@inline(__always)
func isBad(_ item: String) -> Bool {
    return item == "photons" || item == "escape pod" || item == "molten lava" ||
           item == "infinite loop" || item == "giant electromagnet"
}
@inline(__always)
func opposite(_ d: String) -> String {
    switch d {
    case "north": return "south"
    case "south": return "north"
    case "west":  return "east"
    case "east":  return "west"
    default: return ""
    }
}
func parseResultCode(_ text: String) -> String? {
    let needle = "You should be able to get in by typing "
    if let r = text.range(of: needle) {
        var s = text[r.upperBound...]
        var digits = ""
        while let ch = s.first, ch.isNumber {
            digits.append(ch); s = s.dropFirst()
        }
        return digits.isEmpty ? nil : digits
    }
    return nil
}
func findPath(from: Int, to: Int, world: [Room]) -> [Int]? {
    var q = [from]
    var seen = Set([from])
    var prev: [Int: Int] = [:]
    while !q.isEmpty {
        let u = q.removeFirst()
        if u == to { break }
        for (_, v) in world[u].connections {
            if v >= 0 && !seen.contains(v) {
                seen.insert(v); prev[v] = u; q.append(v)
            }
        }
    }
    guard seen.contains(to) else { return nil }
    var path = [to]; var cur = to
    while cur != from { cur = prev[cur]!; path.append(cur) }
    return path.reversed()
}

// MARK: - Main

let inputURL = URL(fileURLWithPath: "input.txt")
guard let raw = try? String(contentsOf: inputURL).trimmingCharacters(in: .whitespacesAndNewlines) else {
    exit(0)
}
let program = raw.split(separator: ",").compactMap { Int64($0) }
let emu = Emulator(program: program)

var world: [Room] = []
var byName: [String: Int] = [:]
var currentIdx: Int? = nil

var inventory: [String: Bool] = [:]
var mode: Mode = .explore
var pathStack: [Int] = []          // DFS backtrack stack (rooms)
var checkpointIdx: Int? = nil
var testDir: String = ""           // direction from checkpoint to floor
var navigateRooms: [Int] = []      // path in NAVIGATE

// last movement context
var lastRoomIdx: Int? = nil
var lastItems: [String] = []
var lastDir: String = ""

// TEST state
var availableItems: [String] = []
var itemMask: Int = 0

var outBuf = Data()

@inline(__always)
func sendCmd(_ s: String) { emu.writeString(s) }

while true {
    let (val, st) = emu.step()
    switch st {
    case .output:
        if let v = val, let u = UnicodeScalar(Int(v)) {
            outBuf.append(String(u).data(using: .utf8)!)
        }
    case .halted:
        let text = String(data: outBuf, encoding: .utf8) ?? ""
        if let code = parseResultCode(text) { print(code) }
        exit(0)
    case .waiting:
        let text = String(data: outBuf, encoding: .utf8) ?? ""
        outBuf.removeAll(keepingCapacity: true)

        if let code = parseResultCode(text) { print(code); exit(0) }

        var itemsHere: [String] = []
        let lines = text.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).trimmingCharacters(in: .whitespacesAndNewlines) }
        var i = 0
        while i < lines.count {
            let line = lines[i]
            if line.isEmpty || line == "Command?" { i += 1; continue }

            // Room header "== Name =="
            if line.hasPrefix("== "), line.hasSuffix(" ==") {
                let name = line.dropFirst(2).dropLast(3).trimmingCharacters(in: .whitespaces)
                i += 1
                // skip description until blank
                while i < lines.count && !lines[i].isEmpty { i += 1 }
                let idx: Int
                if let ex = byName[name] { idx = ex }
                else {
                    idx = world.count
                    world.append(Room(String(name)))
                    byName[String(name)] = idx
                }
                currentIdx = idx
                itemsHere.removeAll()
                continue
            }

            // Doors here lead:
            if line == "Doors here lead:" {
                i += 1
                while i < lines.count, lines[i].hasPrefix("- ") {
                    if let ci = currentIdx {
                        let dir = String(lines[i].dropFirst(2)).trimmingCharacters(in: .whitespaces)
                        if world[ci].connections[dir] == nil { world[ci].connections[dir] = -1 }
                    }
                    i += 1
                }
                continue
            }

            // Items here:
            if line == "Items here:" {
                i += 1
                while i < lines.count, lines[i].hasPrefix("- ") {
                    let item = String(lines[i].dropFirst(2)).trimmingCharacters(in: .whitespaces)
                    itemsHere.append(item); i += 1
                }
                continue
            }

            // You take/drop echoes
            if line.hasPrefix("You take the "), line.hasSuffix(".") {
                let item = String(line.dropFirst("You take the ".count).dropLast(1))
                inventory[item] = true
                if lastRoomIdx != nil && !lastItems.isEmpty {
                    lastItems.removeAll { $0 == item }
                }
                i += 1; continue
            }
            if line.hasPrefix("You drop the "), line.hasSuffix(".") {
                let item = String(line.dropFirst("You drop the ".count).dropLast(1))
                inventory[item] = false
                if lastRoomIdx != nil { lastItems.append(item) }
                i += 1; continue
            }

            // Alert! ejection -> learn checkpoint and testDir
            if line.hasPrefix("A loud, robotic voice says \"Alert!") {
                if mode == .explore {
                    if !pathStack.isEmpty { _ = pathStack.popLast() }
                    if let cp = lastRoomIdx, let cur = currentIdx, !lastDir.isEmpty {
                        checkpointIdx = cp
                        testDir = lastDir
                        world[cp].connections[lastDir] = cur
                        let rev = opposite(lastDir)
                        world[cur].connections[rev] = cp
                    }
                }
                lastRoomIdx = nil; lastItems.removeAll(); lastDir = ""
                i += 1; continue
            }

            i += 1
        }

        // Wire last movement if needed
        if let prev = lastRoomIdx, let cur = currentIdx, !lastDir.isEmpty {
            if world[prev].connections[lastDir] == nil || world[prev].connections[lastDir] == -1 {
                world[prev].connections[lastDir] = cur
                let rev = opposite(lastDir)
                world[cur].connections[rev] = prev
            }
        }

        // Update last context
        lastRoomIdx = currentIdx
        lastItems = itemsHere
        lastDir = ""

        // Decide next action (exactly one command)
        switch mode {
        case .explore:
            // take one safe item if present
            if let pick = itemsHere.first(where: { !isBad($0) }) {
                sendCmd("take \(pick)\n"); continue
            }
            // unexplored door?
            if let ci = currentIdx {
                if let (dir, _) = world[ci].connections.first(where: { $0.value == -1 }) {
                    pathStack.append(ci)
                    lastDir = dir
                    sendCmd("\(dir)\n"); continue
                }
            }
            // backtrack
            if let ci = currentIdx, let backRoom = pathStack.popLast() {
                if let (dir, _) = world[ci].connections.first(where: { $0.value == backRoom }) {
                    lastDir = dir
                    sendCmd("\(dir)\n"); continue
                }
            }
            // navigate to checkpoint if known
            if let ci = currentIdx, let cp = checkpointIdx {
                if ci != cp, let p = findPath(from: ci, to: cp, world: world) {
                    navigateRooms = Array(p.dropFirst()) // skip current
                    mode = .navigate; continue
                } else if ci == cp {
                    mode = .test
                    availableItems = inventory.compactMap { $0.value ? $0.key : nil }.sorted()
                    itemMask = 0
                }
            }
        case .navigate:
            if let ci = currentIdx {
                if navigateRooms.isEmpty {
                    mode = .test
                    availableItems = inventory.compactMap { $0.value ? $0.key : nil }.sorted()
                    itemMask = 0
                } else {
                    let nextRoom = navigateRooms.removeFirst()
                    if let (dir, _) = world[ci].connections.first(where: { $0.value == nextRoom }) {
                        lastDir = dir
                        sendCmd("\(dir)\n"); continue
                    } else {
                        mode = .explore // fallback
                    }
                }
            }
        case .test:
            // Align inventory to current mask (toggle one item per cycle)
            var changed = false
            for (idx, it) in availableItems.enumerated() {
                let want = ((itemMask >> idx) & 1) == 1
                let have = inventory[it] ?? false
                if want != have {
                    sendCmd("\(want ? "take" : "drop") \(it)\n")
                    changed = true
                    break
                }
            }
            if changed { continue }
            // Try crossing the floor
            if !testDir.isEmpty {
                sendCmd("\(testDir)\n")
                itemMask += 1
                continue
            } else {
                // discover by probing any door if somehow missing
                if let ci = currentIdx, let (dir, _) = world[ci].connections.first {
                    lastDir = dir; sendCmd("\(dir)\n"); continue
                } else { exit(0) }
            }
        }
    }
}
