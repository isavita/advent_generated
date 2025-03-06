
import Foundation

struct Board {
    var numbers: [[Int]]
    var marked: [[Bool]]

    init(numbers: [[Int]]) {
        self.numbers = numbers
        self.marked = Array(repeating: Array(repeating: false, count: numbers[0].count), count: numbers.count)
    }

    mutating func mark(number: Int) {
        for i in 0..<numbers.count {
            for j in 0..<numbers[i].count {
                if numbers[i][j] == number {
                    marked[i][j] = true
                }
            }
        }
    }

    func hasWon() -> Bool {
        // Check rows
        for row in marked {
            if row.allSatisfy({ $0 }) {
                return true
            }
        }

        // Check columns
        for j in 0..<marked[0].count {
            var columnMarked = true
            for i in 0..<marked.count {
                if !marked[i][j] {
                    columnMarked = false
                    break
                }
            }
            if columnMarked {
                return true
            }
        }

        return false
    }

    func sumOfUnmarked() -> Int {
        var sum = 0
        for i in 0..<numbers.count {
            for j in 0..<numbers[i].count {
                if !marked[i][j] {
                    sum += numbers[i][j]
                }
            }
        }
        return sum
    }
}

func solveBingo(draws: [Int], boards: inout [Board]) -> Int? {
    
    for draw in draws {
        for index in 0..<boards.count {
            boards[index].mark(number: draw)
            
            if boards[index].hasWon() {
                return boards[index].sumOfUnmarked() * draw
            }
        }
    }
    return nil
}



func main() {
    do {
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = input.components(separatedBy: "\n\n")
        
        let draws = lines[0].split(separator: ",").compactMap { Int($0) }
        
        var boards: [Board] = []
        for boardString in lines.dropFirst() {
            let rows = boardString.split(separator: "\n").map {
                $0.split(separator: " ").compactMap { Int($0) }
            }
            boards.append(Board(numbers: rows))
        }

        if let result = solveBingo(draws: draws, boards: &boards) {
            print(result)
        } else {
            print("No winning board found.")
        }


    } catch {
        print("Error reading file: \(error)")
    }
}


main()
