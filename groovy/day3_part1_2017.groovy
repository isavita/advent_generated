def input = new File("input.txt").text.toInteger()

def size = Math.ceil(Math.sqrt(input)).toInteger()
def center = (size - 1) / 2

def x = size - 1
def y = size - 1

def current = size * size
def distance = 0

while (current != input) {
    if (current - size + 1 < input && input <= current) {
        x--
    } else if (current - 2 * (size - 1) < input && input <= current - size + 1) {
        y--
    } else if (current - 3 * (size - 1) < input && input <= current - 2 * (size - 1)) {
        x++
    } else {
        y++
    }

    current--
    distance++
}

def result = Math.abs(center - x) + Math.abs(center - y)
println result