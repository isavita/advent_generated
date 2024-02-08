
def input = new File("input.txt").text.tokenize('\n')*.toInteger()
def index = 0
def steps = 0

while (index >= 0 && index < input.size()) {
    def jump = input[index]
    input[index] += jump >= 3 ? -1 : 1
    index += jump
    steps++
}

println steps
