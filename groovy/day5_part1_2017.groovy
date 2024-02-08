
def data = new File("input.txt").text.trim().split("\n").collect { it as Integer }

def index = 0
def steps = 0

while (index >= 0 && index < data.size()) {
    def jump = data[index]
    data[index]++
    index += jump
    steps++
}

println steps
