
def password = "abcdefgh"
new File("input.txt").eachLine { op ->
    def parts = op.split()
    switch (parts[0]) {
        case "swap":
            if (parts[1] == "position") {
                def x = parts[2].toInteger()
                def y = parts[5].toInteger()
                password = swapPosition(password, x, y)
            } else if (parts[1] == "letter") {
                def x = parts[2]
                def y = parts[5]
                password = swapLetter(password, x, y)
            }
            break
        case "rotate":
            if (parts[1] == "left") {
                def steps = parts[2].toInteger()
                password = rotateLeft(password, steps)
            } else if (parts[1] == "right") {
                def steps = parts[2].toInteger()
                password = rotateRight(password, steps)
            } else if (parts[1] == "based") {
                def x = parts[6]
                password = rotateBasedOnPosition(password, x)
            }
            break
        case "reverse":
            def x = parts[2].toInteger()
            def y = parts[4].toInteger()
            password = reversePositions(password, x, y)
            break
        case "move":
            def x = parts[2].toInteger()
            def y = parts[5].toInteger()
            password = movePosition(password, x, y)
            break
    }
}
println password

def swapPosition(password, x, y) {
    def chars = password.toCharArray()
    def temp = chars[x]
    chars[x] = chars[y]
    chars[y] = temp
    return new String(chars)
}

def swapLetter(password, x, y) {
    password.replace(x, "#TEMP#").replace(y, x).replace("#TEMP#", y)
}

def rotateLeft(password, steps) {
    steps = steps % password.size()
    password[steps..-1] + password[0..<steps]
}

def rotateRight(password, steps) {
    steps = steps % password.size()
    password[-steps..-1] + password[0..<-steps]
}

def rotateBasedOnPosition(password, x) {
    def index = password.indexOf(x)
    def steps = 1 + index
    if (index >= 4) {
        steps++
    }
    rotateRight(password, steps)
}

def reversePositions(password, x, y) {
    def chars = password.toCharArray()
    while (x < y) {
        def temp = chars[x]
        chars[x] = chars[y]
        chars[y] = temp
        x++
        y--
    }
    return new String(chars)
}

def movePosition(password, x, y) {
    def chars = password.toCharArray()
    def charToMove = chars[x]
    def list = chars.toList()
    list.remove(x)
    list.add(y, charToMove)
    return list.join()
}
