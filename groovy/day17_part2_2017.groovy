
def input = new File("input.txt").text.toInteger()

def buffer = [0]
def currentPosition = 0

(1..2017).each { i ->
    currentPosition = (currentPosition + input) % buffer.size() + 1
    buffer.add(currentPosition, i)
}

println buffer[(buffer.indexOf(2017) + 1) % buffer.size()]

def result = 0
(1..50000000).each { i ->
    currentPosition = (currentPosition + input) % i + 1
    if (currentPosition == 1) {
        result = i
    }
}

println result
