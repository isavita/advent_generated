def input = new File("input.txt").text.tokenize(',').collect { it.toInteger() }

def map = [:].withDefault { [-1, -1] }
def lastNum = -1
def turn = 1

input.each { num ->
    map[num] = [turn, -1]
    lastNum = num
    turn++
}

while (turn <= 2020) {
    def nextNum = map[lastNum][1] == -1 ? 0 : map[lastNum][0] - map[lastNum][1]
    map[nextNum] = [turn, map[nextNum][0]]
    lastNum = nextNum
    turn++
}

println(lastNum)