def input = new File("input.txt").text

def floor = 0
def position = 0
def basementPosition = null

input.each {
    position++
    if (it == '(') {
        floor++
    } else if (it == ')') {
        floor--
    }
    
    if (basementPosition == null && floor == -1) {
        basementPosition = position
    }
}

println "Part One: $floor"
println "Part Two: $basementPosition"