@Grab('commons-io:commons-io:2.6')
import org.apache.commons.io.FileUtils

class Cart {
    int x
    int y
    char direction
    int turns
}

class Position {
    int x
    int y
}

def tracks = []
def carts = []
def file = new File('input.txt')
def lines = file.text.split('\n')

lines.eachWithIndex { line, y ->
    def trackLine = line.toCharArray()
    trackLine.eachWithIndex { char c, x ->
        switch (c) {
            case '>' :
            case '<' :
            case '^' :
            case 'v' :
                carts << new Cart(x: x, y: y, direction: c, turns: 0)
                if (c == '>' || c == '<') {
                    trackLine[x] = '-'
                } else {
                    trackLine[x] = '|'
                }
                break
            default :
                trackLine[x] = c
        }
    }
    tracks << trackLine
}

while (carts.size() > 1) {
    carts.sort { a, b -> a.y == b.y ? a.x <=> b.x : a.y <=> b.y }

    def toRemove = [:]
    carts.eachWithIndex { cart, i ->
        if (toRemove.containsKey(i)) return
        moveCart(cart, tracks)
        def crashIndex = checkCrash(cart, carts)
        if (crashIndex != -1) {
            toRemove[i] = true
            toRemove[crashIndex] = true
        }
    }

    carts = carts.findAll { !toRemove.containsKey(carts.indexOf(it)) }
}

println "${carts[0].x},${carts[0].y}"

def moveCart(Cart cart, tracks) {
    switch (cart.direction) {
        case '>' :
            cart.x++
            break
        case '<' :
            cart.x--
            break
        case '^' :
            cart.y--
            break
        case 'v' :
            cart.y++
            break
    }

    switch (tracks[cart.y][cart.x]) {
        case '+' :
            turnCart(cart)
            break
        case '/' :
        case '\\' :
            changeDirection(cart, tracks[cart.y][cart.x])
            break
    }
}

def turnCart(Cart cart) {
    if (cart.turns % 3 == 0) {
        switch (cart.direction) {
            case '>' :
                cart.direction = '^'
                break
            case '<' :
                cart.direction = 'v'
                break
            case '^' :
                cart.direction = '<'
                break
            case 'v' :
                cart.direction = '>'
                break
        }
    } else if (cart.turns % 3 == 2) {
        switch (cart.direction) {
            case '>' :
                cart.direction = 'v'
                break
            case '<' :
                cart.direction = '^'
                break
            case '^' :
                cart.direction = '>'
                break
            case 'v' :
                cart.direction = '<'
                break
        }
    }
    cart.turns++
}

def changeDirection(Cart cart, char track) {
    if (track == '/') {
        switch (cart.direction) {
            case '>' :
                cart.direction = '^'
                break
            case '<' :
                cart.direction = 'v'
                break
            case '^' :
                cart.direction = '>'
                break
            case 'v' :
                cart.direction = '<'
                break
        }
    } else if (track == '\\') {
        switch (cart.direction) {
            case '>' :
                cart.direction = 'v'
                break
            case '<' :
                cart.direction = '^'
                break
            case '^' :
                cart.direction = '<'
                break
            case 'v' :
                cart.direction = '>'
                break
        }
    }
}

def checkCrash(Cart cart, List<Cart> carts) {
    for (int i = 0; i < carts.size(); i++) {
        if (carts[i] != cart && carts[i].x == cart.x && carts[i].y == cart.y) {
            return i
        }
    }
    return -1
}