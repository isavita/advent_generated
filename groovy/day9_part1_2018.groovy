def input = new File("input.txt").text.tokenize("\n")

def players = input[0].split(" ")[0].toInteger()
def lastMarble = input[0].split(" ")[6].toInteger()

def scores = new long[players]
def circle = [0]
def currentPosition = 0

(1..lastMarble).each { marble ->
    if (marble % 23 == 0) {
        currentPosition = ((currentPosition - 7) % circle.size() + circle.size()) % circle.size()
        scores[marble % players] += marble + circle.remove(currentPosition)
    } else {
        currentPosition = ((currentPosition + 2) % circle.size() + circle.size()) % circle.size()
        circle.add(currentPosition, marble)
    }
}

println(scores.max())