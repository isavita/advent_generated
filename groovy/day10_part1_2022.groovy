
def x = [1]
new File("input.txt").eachLine { line ->
    switch (line) {
        case "noop":
            x << x.last()
            break
        default:
            def n = line.split(" ")[1] as Integer
            x << x.last()
            x << x.last() + n
            break
    }
}

def sum = 0
x.eachWithIndex { value, index ->
    if ((index - 19) % 40 == 0) {
        sum += (index + 1) * value
    }
}
println sum
