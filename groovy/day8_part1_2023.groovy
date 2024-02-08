
def file = new File("input.txt")
def input = file.text.trim()

def re = ~/[A-Z]{3}/

def lines = input.split("\n")

def desertMap = [:]

lines[2..-1].each { line ->
    if (line.isEmpty()) {
        return
    }
    
    def matches = (line =~ re)
    desertMap[matches[0]] = [left: matches[1], right: matches[2]]
}

def current = "AAA"
def steps = 0

while (current != "ZZZ") {
    lines[0].each { direction ->
        if (direction == 'R') {
            current = desertMap[current].right
        } else if (direction == 'L') {
            current = desertMap[current].left
        }
        steps++

        if (current == "ZZZ") {
            return
        }
    }
}

println steps
