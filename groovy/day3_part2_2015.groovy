def inputFile = new File("input.txt")
def directions = inputFile.text

def visitedHouses = [[0,0]]
def santaPos = [0,0]
def roboSantaPos = [0,0]

def isSantaTurn = true

directions.each {
    def move = it
    def pos = isSantaTurn ? santaPos : roboSantaPos

    switch(move) {
        case '^':
            pos[1]++
            break
        case 'v':
            pos[1]--
            break
        case '>':
            pos[0]++
            break
        case '<':
            pos[0]--
            break
    }

    if (!visitedHouses.contains(pos)) {
        visitedHouses.add(pos.clone())
    }

    isSantaTurn = !isSantaTurn
}

println visitedHouses.size()