
class Elf {
    int x, y
    int proposedX, proposedY

    Elf(int x, int y) {
        this.x = x
        this.y = y
    }

    void proposeMove(Set<String> elfPositions, List<String> directions) {
        if (adjacentElves(elfPositions).isEmpty()) {
            proposedX = x
            proposedY = y
            return // No move needed
        }

        for (String direction : directions) {
            switch (direction) {
                case 'N':
                    if (!elfPositions.contains("${x - 1},${y - 1}") &&
                        !elfPositions.contains("${x},${y - 1}") &&
                        !elfPositions.contains("${x + 1},${y - 1}")) {
                        proposedX = x
                        proposedY = y - 1
                        return
                    }
                    break
                case 'S':
                    if (!elfPositions.contains("${x - 1},${y + 1}") &&
                        !elfPositions.contains("${x},${y + 1}") &&
                        !elfPositions.contains("${x + 1},${y + 1}")) {
                        proposedX = x
                        proposedY = y + 1
                        return
                    }
                    break
                case 'W':
                    if (!elfPositions.contains("${x - 1},${y - 1}") &&
                        !elfPositions.contains("${x - 1},${y}") &&
                        !elfPositions.contains("${x - 1},${y + 1}")) {
                        proposedX = x - 1
                        proposedY = y
                        return
                    }
                    break
                case 'E':
                    if (!elfPositions.contains("${x + 1},${y - 1}") &&
                        !elfPositions.contains("${x + 1},${y}") &&
                        !elfPositions.contains("${x + 1},${y + 1}")) {
                        proposedX = x + 1
                        proposedY = y
                        return
                    }
                    break
            }
        }

        // No valid move found
        proposedX = x
        proposedY = y
    }


    Set<String> adjacentElves(Set<String> elfPositions) {
        Set<String> adjacent = new HashSet<>()
        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                if (dx == 0 && dy == 0) continue
                if (elfPositions.contains("${x + dx},${y + dy}")) {
                    adjacent.add("${x + dx},${y + dy}")
                }
            }
        }
        return adjacent
    }

    void move() {
        x = proposedX
        y = proposedY
    }
    
    @Override
    String toString() {
        return "($x, $y)"
    }
}

def simulate(List<String> lines, int rounds) {
    Set<Elf> elves = new HashSet<>()
    for (int y = 0; y < lines.size(); y++) {
        for (int x = 0; x < lines.get(y).length(); x++) {
            if (lines.get(y).charAt(x) == '#') {
                elves.add(new Elf(x, y))
            }
        }
    }

    List<String> directions = ['N', 'S', 'W', 'E']

    for (int round = 0; round < rounds; round++) {
        Set<String> elfPositions = elves.collect { "${it.x},${it.y}" } as Set
        Map<String, List<Elf>> proposedMoves = new HashMap<>()

        // Propose moves
        for (Elf elf : elves) {
            elf.proposeMove(elfPositions, directions)
            String proposedPos = "${elf.proposedX},${elf.proposedY}"
            if (!proposedMoves.containsKey(proposedPos)) {
                proposedMoves.put(proposedPos, new ArrayList<>())
            }
            proposedMoves.get(proposedPos).add(elf)
        }

        // Execute moves
        for (List<Elf> elvesAtPos : proposedMoves.values()) {
            if (elvesAtPos.size() == 1) {
                elvesAtPos.get(0).move()
            }
        }

        // Rotate directions
        directions.add(directions.remove(0))
    }
    return elves
}

def calculateEmptyTiles(Set<Elf> elves) {
     if (elves.isEmpty()) {
        return 0; 
    }

    int minX = elves.stream().mapToInt { it.x }.min().getAsInt()
    int maxX = elves.stream().mapToInt { it.x }.max().getAsInt()
    int minY = elves.stream().mapToInt { it.y }.min().getAsInt()
    int maxY = elves.stream().mapToInt { it.y }.max().getAsInt()

    return (maxX - minX + 1) * (maxY - minY + 1) - elves.size()
}


def main(args) {
    def file = new File("input.txt")
    
    if (!file.exists()) {
        println "File input.txt not found."
        return
    }
    
    List<String> lines = file.readLines()

    def elves = simulate(lines, 10)
    def emptyTiles = calculateEmptyTiles(elves)
    println emptyTiles
}

main([])
