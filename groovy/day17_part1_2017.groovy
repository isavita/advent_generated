
def steps = new File("input.txt").text.toInteger()
def buffer = [0]
def currentPos = 0

(1..2017).each { i ->
    currentPos = (currentPos + steps) % buffer.size()
    buffer.add(currentPos + 1, i)
    currentPos++
}

def index = buffer.indexOf(2017)
println buffer[(index + 1) % buffer.size()]
