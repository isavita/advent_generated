
import Foundation

struct GameState {
    var playerHP, playerMana, bossHP, bossDamage: Int
    var shieldTimer, poisonTimer, rechargeTimer: Int
    var manaSpent: Int
}

func minManaToWin(initialState: GameState) -> Int {
    var minMana = Int.max

    func simulate(state: GameState, playerTurn: Bool) {
        if state.manaSpent >= minMana {
            return
        }
        if state.bossHP <= 0 {
            minMana = state.manaSpent
            return
        }
        if state.playerHP <= 0 {
            return
        }

        var mutableState = state
        if playerTurn {
            mutableState.playerHP -= 1
            if mutableState.playerHP <= 0 {
                return
            }
        }

        if mutableState.shieldTimer > 0 {
            mutableState.shieldTimer -= 1
        }
        if mutableState.poisonTimer > 0 {
            mutableState.bossHP -= 3
            mutableState.poisonTimer -= 1
        }
        if mutableState.rechargeTimer > 0 {
            mutableState.playerMana += 101
            mutableState.rechargeTimer -= 1
        }

        if !playerTurn {
            var damage = mutableState.bossDamage
            if mutableState.shieldTimer > 0 {
                damage -= 7
            }
            damage = max(1, damage)
            mutableState.playerHP -= damage
            simulate(state: mutableState, playerTurn: true)
            return
        }

        if mutableState.playerMana >= 53 {
            var newState = mutableState
            newState.playerMana -= 53
            newState.manaSpent += 53
            newState.bossHP -= 4
            simulate(state: newState, playerTurn: false)
        }
        if mutableState.playerMana >= 73 {
            var newState = mutableState
            newState.playerMana -= 73
            newState.manaSpent += 73
            newState.bossHP -= 2
            newState.playerHP += 2
            simulate(state: newState, playerTurn: false)
        }
        if mutableState.playerMana >= 113 && mutableState.shieldTimer == 0 {
            var newState = mutableState
            newState.playerMana -= 113
            newState.manaSpent += 113
            newState.shieldTimer = 6
            simulate(state: newState, playerTurn: false)
        }
        if mutableState.playerMana >= 173 && mutableState.poisonTimer == 0 {
            var newState = mutableState
            newState.playerMana -= 173
            newState.manaSpent += 173
            newState.poisonTimer = 6
            simulate(state: newState, playerTurn: false)
        }
        if mutableState.playerMana >= 229 && mutableState.rechargeTimer == 0 {
            var newState = mutableState
            newState.playerMana -= 229
            newState.manaSpent += 229
            newState.rechargeTimer = 5
            simulate(state: newState, playerTurn: false)
        }
    }

    var startState = initialState
    startState.playerHP = 50
    startState.playerMana = 500
    simulate(state: startState, playerTurn: true)
    return minMana
}

let fileURL = URL(fileURLWithPath: "input.txt")
let fileContents = try! String(contentsOf: fileURL)
let lines = fileContents.components(separatedBy: "\n")
let bossHP = Int(lines[0].components(separatedBy: ": ")[1])!
let bossDamage = Int(lines[1].components(separatedBy: ": ")[1])!

let initialState = GameState(playerHP: 0, playerMana: 0, bossHP: bossHP, bossDamage: bossDamage, shieldTimer: 0, poisonTimer: 0, rechargeTimer: 0, manaSpent: 0)
print(minManaToWin(initialState: initialState))
