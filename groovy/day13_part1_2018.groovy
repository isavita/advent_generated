def track = []
def carts = []

new File("input.txt").eachLine { line ->
    def row = line.toCharArray()
    track << row
    for (int j = 0; j < row.size(); j++) {
        switch (row[j]) {
            case '>':
                row[j] = '-'
                carts << [x: j, y: track.size() - 1, dir: '>', turn: 0]
                break
            case '<':
                row[j] = '-'
                carts << [x: j, y: track.size() - 1, dir: '<', turn: 0]
                break
            case '^':
                row[j] = '|'
                carts << [x: j, y: track.size() - 1, dir: '^', turn: 0]
                break
            case 'v':
                row[j] = '|'
                carts << [x: j, y: track.size() - 1, dir: 'v', turn: 0]
                break
        }
    }
}

def collision = false
while (!collision) {
    carts.each { cart ->
        switch (cart.dir) {
            case '>':
                cart = movingRight(track, cart)
                break
            case '<':
                cart = movingLeft(track, cart)
                break
            case '^':
                cart = movingUp(track, cart)
                break
            case 'v':
                cart = movingDown(track, cart)
                break
        }
    }

    for (int i = 0; i < carts.size(); i++) {
        for (int j = i + 1; j < carts.size(); j++) {
            if (carts[i].x == carts[j].x && carts[i].y == carts[j].y) {
                collision = true
                println "${carts[i].x},${carts[i].y}"
            }
        }
    }
}

def movingDown(track, cart) {
    switch (track[cart.y + 1][cart.x]) {
        case '/':
            cart.dir = '<'
            break
        case '\\':
            cart.dir = '>'
            break
        case '+':
            if (cart.turn == 0) {
                cart.dir = '>'
                cart.turn = 1
            } else if (cart.turn == 1) {
                cart.turn = 2
            } else if (cart.turn == 2) {
                cart.dir = '<'
                cart.turn = 0
            }
            break
        case '|':
            break
        default:
            println "Error on track cart can't move : ${cart.x} ${cart.y - 1} ${track[cart.y - 1][cart.x]}"
    }
    cart.y = cart.y + 1
    cart
}

def movingUp(track, cart) {
    switch (track[cart.y - 1][cart.x]) {
        case '/':
            cart.dir = '>'
            break
        case '\\':
            cart.dir = '<'
            break
        case '+':
            if (cart.turn == 0) {
                cart.dir = '<'
                cart.turn = 1
            } else if (cart.turn == 1) {
                cart.turn = 2
            } else if (cart.turn == 2) {
                cart.dir = '>'
                cart.turn = 0
            }
            break
        case '|':
            break
        default:
            println "Error on track cart can't move : ${cart.x} ${cart.y - 1} ${track[cart.y - 1][cart.x]}"
    }
    cart.y = cart.y - 1
    cart
}

def movingLeft(track, cart) {
    switch (track[cart.y][cart.x - 1]) {
        case '/':
            cart.dir = 'v'
            break
        case '\\':
            cart.dir = '^'
            break
        case '+':
            if (cart.turn == 0) {
                cart.dir = 'v'
                cart.turn = 1
            } else if (cart.turn == 1) {
                cart.turn = 2
            } else if (cart.turn == 2) {
                cart.dir = '^'
                cart.turn = 0
            }
            break
        case '-':
            break
        default:
            println "Error on track cart can't move : ${cart.x - 1} ${cart.y} ${track[cart.y][cart.x - 1]}"
    }
    cart.x = cart.x - 1
    cart
}

def movingRight(track, cart) {
    switch (track[cart.y][cart.x + 1]) {
        case '\\':
            cart.dir = 'v'
            break
        case '/':
            cart.dir = '^'
            break
        case '+':
            if (cart.turn == 0) {
                cart.dir = '^'
                cart.turn = 1
            } else if (cart.turn == 1) {
                cart.turn = 2
            } else if (cart.turn == 2) {
                cart.dir = 'v'
                cart.turn = 0
            }
            break
        case '-':
            break
        default:
            println "Error on track cart can't move : ${cart.x + 1} ${cart.y} ${track[cart.y][cart.x + 1]}"
    }
    cart.x = cart.x + 1
    cart
}