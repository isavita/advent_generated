
class DiracDice {

    static class GameState {
        int p1Pos, p2Pos, p1Score, p2Score, nextPlayer
        long universes

        GameState(int p1Pos, int p2Pos, int p1Score, int p2Score, int nextPlayer, long universes) {
            this.p1Pos = p1Pos
            this.p2Pos = p2Pos
            this.p1Score = p1Score
            this.p2Score = p2Score
            this.nextPlayer = nextPlayer
            this.universes = universes
        }
    }

    static void main(String[] args) {
        def lines = new File("input.txt").readLines()
        def p1Start = Integer.parseInt(lines[0].split(": ")[1])
        def p2Start = Integer.parseInt(lines[1].split(": ")[1])

        println "Part 1: ${part1(p1Start, p2Start)}"
        println "Part 2: ${part2(p1Start, p2Start)}"
    }

    static long part1(int p1Start, int p2Start) {
        int p1Pos = p1Start
        int p2Pos = p2Start
        int p1Score = 0
        int p2Score = 0
        int die = 1
        int rolls = 0

        while (true) {
            // Player 1's turn
            int move = 0
            for (int i = 0; i < 3; i++) {
                move += die
                die = (die % 100) + 1
            }
            rolls += 3
            p1Pos = (p1Pos + move - 1) % 10 + 1
            p1Score += p1Pos
            if (p1Score >= 1000) {
                return p2Score * rolls
            }

            // Player 2's turn
            move = 0
            for (int i = 0; i < 3; i++) {
                move += die
                die = (die % 100) + 1
            }
            rolls += 3
            p2Pos = (p2Pos + move - 1) % 10 + 1
            p2Score += p2Pos
            if (p2Score >= 1000) {
                return p1Score * rolls
            }
        }
    }

    static long part2(int p1Start, int p2Start) {
        def wins = [0L, 0L] // Player 1 wins, Player 2 wins
        def queue = new LinkedList<GameState>()
        queue.add(new GameState(p1Start, p2Start, 0, 0, 1, 1)) // Start with player 1

        def diracRolls = [:]
        for(int i = 1; i <= 3; i++){
            for(int j = 1; j <=3; j++){
                for(int k = 1; k <= 3; k++){
                    diracRolls[i+j+k] = (diracRolls[i+j+k] ?: 0) + 1
                }
            }
        }

        while (!queue.isEmpty()) {
            def currentState = queue.poll()

            if (currentState.nextPlayer == 1) {
                diracRolls.each { roll, universes ->
                    def newPos = (currentState.p1Pos + roll - 1) % 10 + 1
                    def newScore = currentState.p1Score + newPos
                    if (newScore >= 21) {
                        wins[0] += currentState.universes * universes
                    } else {
                        queue.add(new GameState(newPos, currentState.p2Pos, newScore, currentState.p2Score, 2, currentState.universes * universes))
                    }
                }
            } else {
                diracRolls.each { roll, universes ->
                    def newPos = (currentState.p2Pos + roll - 1) % 10 + 1
                    def newScore = currentState.p2Score + newPos
                    if (newScore >= 21) {
                        wins[1] += currentState.universes * universes
                    } else {
                        queue.add(new GameState(currentState.p1Pos, newPos, currentState.p1Score, newScore, 1, currentState.universes * universes))
                    }
                }
            }
        }

        return Math.max(wins[0], wins[1])
    }
}
