
class Day23 {

    static void main(String[] args) {
        def inputFile = new File("input.txt")
        if (!inputFile.exists()) {
            println "Error: input.txt does not exist."
            return
        }

        def lines = inputFile.readLines()
        def (elves, part1) = simulate(lines, 10)
        println "Part 1: ${part1}"

        def (_, part2) = simulate(lines, -1) // -1 indicates to run until no moves
        println "Part 2: ${part2}"
    }


    static Tuple2<Set<List<Integer>>, Integer> simulate(List<String> lines, int maxRounds) {
        def elves = new HashSet<List<Integer>>()
        for (int y = 0; y < lines.size(); y++) {
            for (int x = 0; x < lines[y].length(); x++) {
                if (lines[y][x] == '#') {
                    elves.add([x, y])
                }
            }
        }

        def directions = [[0, -1], [0, 1], [-1, 0], [1, 0]] // N, S, W, E

        int round = 0;
        boolean moved
        do {
            moved = false
            round++
            def proposedMoves = [:]
            def moveCounts = [:]

            for (elf in elves) {
                def (x, y) = elf
                boolean hasNeighbors = false;

                // Check all neighbors
                for (int dx = -1; dx <= 1; dx++) {
                    for (int dy = -1; dy <= 1; dy++) {
                        if (dx == 0 && dy == 0) continue;
                        if (elves.contains([x + dx, y + dy])) {
                            hasNeighbors = true;
                            break;
                        }
                    }
                    if(hasNeighbors) break;
                }
                
                if (!hasNeighbors) continue;

                boolean proposed = false;
                for (int i = 0; i < 4 && !proposed; i++) {
                    def dirIndex = (i + (round - 1) % 4) % 4
                    def (dx, dy) = directions[dirIndex]

                    boolean canMove = true;
                    if (dirIndex == 0) { // North
                        for (int ddx = -1; ddx <= 1; ddx++) {
                            if (elves.contains([x + ddx, y + dy])) {
                                canMove = false;
                                break
                            }
                        }
                    } else if (dirIndex == 1) { // South
                        for (int ddx = -1; ddx <= 1; ddx++) {
                            if (elves.contains([x + ddx, y + dy])) {
                                canMove = false;
                                break
                            }
                        }
                    } else if (dirIndex == 2) { //West
                         for (int ddy = -1; ddy <= 1; ddy++) {
                            if (elves.contains([x + dx, y + ddy])) {
                                canMove = false;
                                break
                            }
                        }

                    }else if (dirIndex == 3) { // East
                        for (int ddy = -1; ddy <= 1; ddy++) {
                            if (elves.contains([x + dx, y + ddy])) {
                                canMove = false;
                                break
                            }
                        }
                    }

                    if (canMove) {
                        def proposedMove = [x + dx, y + dy]
                        proposedMoves[elf] = proposedMove
                        moveCounts[proposedMove] = (moveCounts[proposedMove] ?: 0) + 1
                        proposed = true;
                    }
                }
            }

            def newElves = new HashSet<List<Integer>>()
            for (elf in elves) {
                if (proposedMoves.containsKey(elf)) {
                    def proposedMove = proposedMoves[elf]
                    if (moveCounts[proposedMove] == 1) {
                        newElves.add(proposedMove)
                        moved = true
                    } else {
                        newElves.add(elf)
                    }
                } else {
                    newElves.add(elf)
                }
            }
            elves = newElves;

            if (maxRounds > 0 && round >= maxRounds) {
                break;
            }

        } while (moved && (maxRounds == -1 || round < maxRounds));

        if(maxRounds > 0 && round == maxRounds){

                def minX = elves.collect { it[0] }.min()
                def maxX = elves.collect { it[0] }.max()
                def minY = elves.collect { it[1] }.min()
                def maxY = elves.collect { it[1] }.max()

                def emptyTiles = (maxX - minX + 1) * (maxY - minY + 1) - elves.size()
                return new Tuple2(elves, emptyTiles)
        }

        return new Tuple2(elves, round)
    }
}
