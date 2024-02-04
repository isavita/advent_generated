
import sys

class GameState:
    def __init__(self, playerHP=0, playerMana=0, bossHP=0, bossDamage=0, shieldTimer=0, poisonTimer=0, rechargeTimer=0, manaSpent=0):
        self.playerHP = playerHP
        self.playerMana = playerMana
        self.bossHP = bossHP
        self.bossDamage = bossDamage
        self.shieldTimer = shieldTimer
        self.poisonTimer = poisonTimer
        self.rechargeTimer = rechargeTimer
        self.manaSpent = manaSpent

def min_mana_to_win(initialState):
    minMana = sys.maxsize
    def simulate(state, playerTurn):
        nonlocal minMana
        if state.manaSpent >= minMana:
            return
        if state.bossHP <= 0:
            minMana = state.manaSpent
            return
        if state.playerHP <= 0:
            return
        
        if playerTurn:
            state.playerHP -= 1
            if state.playerHP <= 0:
                return
        
        if state.shieldTimer > 0:
            state.shieldTimer -= 1
        if state.poisonTimer > 0:
            state.bossHP -= 3
            state.poisonTimer -= 1
        if state.rechargeTimer > 0:
            state.playerMana += 101
            state.rechargeTimer -= 1
        
        if not playerTurn:
            damage = state.bossDamage
            if state.shieldTimer > 0:
                damage -= 7
            if damage < 1:
                damage = 1
            state.playerHP -= damage
            simulate(state, True)
            return
        
        if state.playerMana >= 53:
            newState = GameState(**state.__dict__)
            newState.playerMana -= 53
            newState.manaSpent += 53
            newState.bossHP -= 4
            simulate(newState, False)
        if state.playerMana >= 73:
            newState = GameState(**state.__dict__)
            newState.playerMana -= 73
            newState.manaSpent += 73
            newState.bossHP -= 2
            newState.playerHP += 2
            simulate(newState, False)
        if state.playerMana >= 113 and state.shieldTimer == 0:
            newState = GameState(**state.__dict__)
            newState.playerMana -= 113
            newState.manaSpent += 113
            newState.shieldTimer = 6
            simulate(newState, False)
        if state.playerMana >= 173 and state.poisonTimer == 0:
            newState = GameState(**state.__dict__)
            newState.playerMana -= 173
            newState.manaSpent += 173
            newState.poisonTimer = 6
            simulate(newState, False)
        if state.playerMana >= 229 and state.rechargeTimer == 0:
            newState = GameState(**state.__dict__)
            newState.playerMana -= 229
            newState.manaSpent += 229
            newState.rechargeTimer = 5
            simulate(newState, False)
    
    initialState.playerHP = 50
    initialState.playerMana = 500
    simulate(initialState, True)
    return minMana

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        lines = file.readlines()
        bossHP = int(lines[0].split(": ")[1])
        bossDamage = int(lines[1].split(": ")[1])

    initialState = GameState(bossHP=bossHP, bossDamage=bossDamage)
    print(min_mana_to_win(initialState))
