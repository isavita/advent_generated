
import Foundation

struct Cart: Hashable {
    var x: Int
    var y: Int
    var direction: Character
    var turns: Int = 0
}

func main() throws {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try String(contentsOf: fileURL)
    let lines = content.split(separator: "\n")

    var tracks: [[Character]] = []
    var carts: Set<Cart> = []

    for (y, line) in lines.enumerated() {
        var trackLine: [Character] = []
        for (x, char) in line.enumerated() {
            switch char {
            case ">", "<", "^", "v":
                carts.insert(Cart(x: x, y: y, direction: char))
                trackLine.append(char == ">" || char == "<" ? "-" : "|")
            default:
                trackLine.append(char)
            }
        }
        tracks.append(trackLine)
    }

    while carts.count > 1 {
        var sortedCarts = carts.sorted {
            if $0.y == $1.y {
                return $0.x < $1.x
            }
            return $0.y < $1.y
        }
        
        var toRemove: Set<Cart> = []
        
        for (i, var cart) in sortedCarts.enumerated() {
            if toRemove.contains(cart) { continue }
            
            moveCart(cart: &cart, tracks: tracks)
            sortedCarts[i] = cart
            
            if let crashIndex = checkCrash(cart: cart, carts: sortedCarts) {
                toRemove.insert(cart)
                toRemove.insert(sortedCarts[crashIndex])
            }
        }
        
        carts = Set(sortedCarts).subtracting(toRemove)
    }

    if let lastCart = carts.first {
        print("\(lastCart.x),\(lastCart.y)")
    }
}

func moveCart(cart: inout Cart, tracks: [[Character]]) {
    switch cart.direction {
    case ">": cart.x += 1
    case "<": cart.x -= 1
    case "^": cart.y -= 1
    case "v": cart.y += 1
    default: break
    }

    switch tracks[cart.y][cart.x] {
    case "+": turnCart(cart: &cart)
    case "/", "\\": changeDirection(cart: &cart, track: tracks[cart.y][cart.x])
    default: break
    }
}

func turnCart(cart: inout Cart) {
    if cart.turns % 3 == 0 {
        switch cart.direction {
        case ">": cart.direction = "^"
        case "<": cart.direction = "v"
        case "^": cart.direction = "<"
        case "v": cart.direction = ">"
        default: break
        }
    } else if cart.turns % 3 == 2 {
        switch cart.direction {
        case ">": cart.direction = "v"
        case "<": cart.direction = "^"
        case "^": cart.direction = ">"
        case "v": cart.direction = "<"
        default: break
        }
    }
    cart.turns += 1
}

func changeDirection(cart: inout Cart, track: Character) {
    if track == "/" {
        switch cart.direction {
        case ">": cart.direction = "^"
        case "<": cart.direction = "v"
        case "^": cart.direction = ">"
        case "v": cart.direction = "<"
        default: break
        }
    } else if track == "\\" {
        switch cart.direction {
        case ">": cart.direction = "v"
        case "<": cart.direction = "^"
        case "^": cart.direction = "<"
        case "v": cart.direction = ">"
        default: break
        }
    }
}

func checkCrash(cart: Cart, carts: [Cart]) -> Int? {
    for (i, c) in carts.enumerated() {
        if c != cart && c.x == cart.x && c.y == cart.y {
            return i
        }
    }
    return nil
}

try! main()
