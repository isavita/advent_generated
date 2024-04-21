import Foundation

func countTrees(in forest: [String], right: Int, down: Int) -> Int {
    var trees = 0
    var x = 0
    let width = forest[0].count

    for y in stride(from: 0, to: forest.count, by: down) {
        if Array(forest[y])[x % width] == "#" {
            trees += 1
        }
        x += right
    }

    return trees
}

do {
    let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
    let forest = fileContent.components(separatedBy: "\n")

    let trees = countTrees(in: forest, right: 3, down: 1)
    print(trees)
} catch {
    print("Error reading file")
}