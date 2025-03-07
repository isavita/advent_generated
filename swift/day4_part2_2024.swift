
import Foundation

func check(d: String) -> Bool {
    return d == "MAS" || d == "SAM"
}

do {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let grid = try String(contentsOf: fileURL, encoding: .utf8)
        .trimmingCharacters(in: .whitespacesAndNewlines)
        .components(separatedBy: .newlines)

    let r = grid.count
    guard let c = grid.first?.count else { exit(0) }

    var count = 0
    for x in 1..<r - 1 {
        for y in 1..<c - 1 {
            let row = Array(grid[x])
            if row[y] == "A" {
                let d1 = String([Array(grid[x-1])[y-1], row[y], Array(grid[x+1])[y+1]])
                let d2 = String([Array(grid[x-1])[y+1], row[y], Array(grid[x+1])[y-1]])
                if check(d: d1) && check(d: d2) {
                    count += 1
                }
            }
        }
    }

    print(count)

} catch {
    print("Error reading input.txt: \(error)")
}
