
import "io" for File
import "os" for Process

class DiracDice {
    static play() {
        var input = File.read("input.txt").trim().split("\n")
        var player1Start = Num.fromString(input[0].split(": ")[1])
        var player2Start = Num.fromString(input[1].split(": ")[1])
        
        var player1Pos = player1Start
        var player2Pos = player2Start
        
        var player1Score = 0
        var player2Score = 0
        
        var dieRoll = 1
        var rollCount = 0
        
        while (true) {
            // Player 1
            var rolls = (dieRoll % 100) + ((dieRoll + 1) % 100) + ((dieRoll + 2) % 100)
            rollCount = rollCount + 3
            dieRoll = dieRoll + 3
            
            player1Pos = ((player1Pos + rolls - 1) % 10) + 1
            player1Score = player1Score + player1Pos
            
            if (player1Score >= 1000) {
                System.print(player2Score * rollCount)
                break
            }
            
            // Player 2
            rolls = (dieRoll % 100) + ((dieRoll + 1) % 100) + ((dieRoll + 2) % 100)
            rollCount = rollCount + 3
            dieRoll = dieRoll + 3
            
            player2Pos = ((player2Pos + rolls - 1) % 10) + 1
            player2Score = player2Score + player2Pos
            
            if (player2Score >= 1000) {
                System.print(player1Score * rollCount)
                break
            }
        }
    }
}

DiracDice.play()
