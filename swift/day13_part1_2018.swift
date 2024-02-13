
import Foundation

let input = try String(contentsOfFile: "input.txt")
var grid = input.components(separatedBy: "\n")
var carts: [(x: Int, y: Int, direction: Character, nextTurn: Int)] = []

for y in 0..<grid.count {
    for x in 0..<grid[y].count {
        let char = grid[y][grid[y].index(grid[y].startIndex, offsetBy: x)]
        if char == "^" || char == "v" || char == "<" || char == ">" {
            carts.append((x: x, y: y, direction: char, nextTurn: 0))
            if char == "^" || char == "v" {
                grid[y] = grid[y].replacingOccurrences(of: String(char), with: "|")
            } else {
                grid[y] = grid[y].replacingOccurrences(of: String(char), with: "-")
            }
        }
    }
}

let directions = ["^": (dx: 0, dy: -1), "v": (dx: 0, dy: 1), "<": (dx: -1, dy: 0), ">": (dx: 1, dy: 0)]
let turns = [0: "L", 1: "S", 2: "R"]

func turnCart(_ cart: inout (x: Int, y: Int, direction: Character, nextTurn: Int)) {
    let currentDirection = cart.direction
    let nextTurn = cart.nextTurn
    let nextDirection = directions[String(currentDirection)]!
    let nextX = cart.x + nextDirection.dx
    let nextY = cart.y + nextDirection.dy
    
    if grid[nextY][grid[nextY].index(grid[nextY].startIndex, offsetBy: nextX)] == "/" {
        if currentDirection == "^" {
            cart.direction = ">"
        } else if currentDirection == "v" {
            cart.direction = "<"
        } else if currentDirection == "<" {
            cart.direction = "v"
        } else if currentDirection == ">" {
            cart.direction = "^"
        }
    } else if grid[nextY][grid[nextY].index(grid[nextY].startIndex, offsetBy: nextX)] == "\\" {
        if currentDirection == "^" {
            cart.direction = "<"
        } else if currentDirection == "v" {
            cart.direction = ">"
        } else if currentDirection == "<" {
            cart.direction = "^"
        } else if currentDirection == ">" {
            cart.direction = "v"
        }
    } else if grid[nextY][grid[nextY].index(grid[nextY].startIndex, offsetBy: nextX)] == "+" {
        let turn = turns[nextTurn % 3]!
        if turn == "L" {
            if currentDirection == "^" {
                cart.direction = "<"
            } else if currentDirection == "v" {
                cart.direction = ">"
            } else if currentDirection == "<" {
                cart.direction = "v"
            } else if currentDirection == ">" {
                cart.direction = "^"
            }
        } else if turn == "R" {
            if currentDirection == "^" {
                cart.direction = ">"
            } else if currentDirection == "v" {
                cart.direction = "<"
            } else if currentDirection == "<" {
                cart.direction = "^"
            } else if currentDirection == ">" {
                cart.direction = "v"
            }
        }
        cart.nextTurn += 1
    }
    cart.x = nextX
    cart.y = nextY
}

while true {
    carts.sort { $0.y == $1.y ? $0.x < $1.x : $0.y < $1.y }
    for i in 0..<carts.count {
        turnCart(&carts[i])
        let cart = carts[i]
        for j in 0..<carts.count {
            if i != j && cart.x == carts[j].x && cart.y == carts[j].y {
                print("\(cart.x),\(cart.y)")
                exit(0)
            }
        }
    }
}
