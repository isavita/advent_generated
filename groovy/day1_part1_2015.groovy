def input = new File("input.txt").text
def floor = 0

input.each {
    if (it == '(') {
        floor++
    } else if (it == ')') {
        floor--
    }
}

println floor