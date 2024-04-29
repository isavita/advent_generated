import java.util.regex.Pattern

class Disc {
    int totalPositions
    int startPosition
}

def discs = []
def file = new File('input.txt')
file.eachLine { line ->
    def matcher = line =~ /Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)./
    if (matcher.matches()) {
        def totalPositions = matcher.group(2) as int
        def startPosition = matcher.group(3) as int
        discs << new Disc(totalPositions: totalPositions, startPosition: startPosition)
    }
}

// Add the new disc as per Part Two's requirement
discs << new Disc(totalPositions: 11, startPosition: 0)

def time = 0
while (true) {
    if (checkDiscs(discs, time)) {
        println time
        break
    }
    time++
}

def checkDiscs(discs, time) {
    for (int i = 0; i < discs.size(); i++) {
        def disc = discs[i]
        def position = (disc.startPosition + time + i + 1) % disc.totalPositions
        if (position != 0) {
            return false
        }
    }
    true
}