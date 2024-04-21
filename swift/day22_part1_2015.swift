import Foundation

struct GameState {
    var playerHP: Int
    var playerMana: Int
    var bossHP: Int
    var bossDamage: Int
    var shieldTimer: Int
    var poisonTimer: Int
    var rechargeTimer: Int
    var manaSpent: Int
}

func minManaToWin(_ state: GameState) -> Int {
    var minMana = Int.max
    func simulate(_ state: GameState, _ playerTurn: Bool) {
        var newState = state
        if newState.manaSpent >= minMana { return }
        if newState.bossHP <= 0 { minMana = newState.manaSpent; return }
        if newState.playerHP <= 0 { return }
        
        if newState.shieldTimer > 0 { newState.shieldTimer -= 1 }
        if newState.poisonTimer > 0 { newState.bossHP -= 3; newState.poisonTimer -= 1 }
        if newState.rechargeTimer > 0 { newState.playerMana += 101; newState.rechargeTimer -= 1 }
        
        if !playerTurn {
            var damage = newState.bossDamage
            if newState.shieldTimer > 0 { damage -= 7 }
            if damage < 1 { damage = 1 }
            newState.playerHP -= damage
            simulate(newState, true)
            return
        }
        
        if newState.playerMana >= 53 {
            var newState2 = newState
            newState2.playerMana -= 53
            newState2.manaSpent += 53
            newState2.bossHP -= 4
            simulate(newState2, false)
        }
        if newState.playerMana >= 73 {
            var newState2 = newState
            newState2.playerMana -= 73
            newState2.manaSpent += 73
            newState2.bossHP -= 2
            newState2.playerHP += 2
            simulate(newState2, false)
        }
        if newState.playerMana >= 113 && newState.shieldTimer == 0 {
            var newState2 = newState
            newState2.playerMana -= 113
            newState2.manaSpent += 113
            newState2.shieldTimer = 6
            simulate(newState2, false)
        }
        if newState.playerMana >= 173 && newState.poisonTimer == 0 {
            var newState2 = newState
            newState2.playerMana -= 173
            newState2.manaSpent += 173
            newState2.poisonTimer = 6
            simulate(newState2, false)
        }
        if newState.playerMana >= 229 && newState.rechargeTimer == 0 {
            var newState2 = newState
            newState2.playerMana -= 229
            newState2.manaSpent += 229
            newState2.rechargeTimer = 5
            simulate(newState2, false)
        }
    }
    
    var initialState = state
    initialState.playerHP = 50
    initialState.playerMana = 500
    simulate(initialState, true)
    return minMana
}

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt")
        let lines = fileContent.components(separatedBy: "\n")
        let bossHP = Int(lines[0].components(separatedBy: ": ")[1])!
        let bossDamage = Int(lines[1].components(separatedBy: ": ")[1])!
        
        let initialState = GameState(playerHP: 50, playerMana: 500, bossHP: bossHP, bossDamage: bossDamage, shieldTimer: 0, poisonTimer: 0, rechargeTimer: 0, manaSpent: 0)
        print(minManaToWin(initialState))
    } catch {
        print("Error reading file")
    }
}

main()