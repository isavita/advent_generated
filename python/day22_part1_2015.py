
import sys

class GameState:
    def __init__(self):
        self.playerHP = 0
        self.playerMana = 0
        self.bossHP = 0
        self.bossDamage = 0
        self.shieldTimer = 0
        self.poisonTimer = 0
        self.rechargeTimer = 0
        self.manaSpent = 0

def min_mana_to_win(initial_state):
    min_mana = sys.maxsize
    def simulate(state, player_turn):
        nonlocal min_mana
        if state.manaSpent >= min_mana:
            return
        if state.bossHP <= 0:
            min_mana = state.manaSpent
            return
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

        if not player_turn:
            damage = state.bossDamage
            if state.shieldTimer > 0:
                damage -= 7
            if damage < 1:
                damage = 1
            state.playerHP -= damage
            simulate(state, True)
            return

        if state.playerMana >= 53:
            new_state = GameState()
            new_state.__dict__ = state.__dict__.copy()
            new_state.playerMana -= 53
            new_state.manaSpent += 53
            new_state.bossHP -= 4
            simulate(new_state, False)
        
        if state.playerMana >= 73:
            new_state = GameState()
            new_state.__dict__ = state.__dict__.copy()
            new_state.playerMana -= 73
            new_state.manaSpent += 73
            new_state.bossHP -= 2
            new_state.playerHP += 2
            simulate(new_state, False)
        
        if state.playerMana >= 113 and state.shieldTimer == 0:
            new_state = GameState()
            new_state.__dict__ = state.__dict__.copy()
            new_state.playerMana -= 113
            new_state.manaSpent += 113
            new_state.shieldTimer = 6
            simulate(new_state, False)
        
        if state.playerMana >= 173 and state.poisonTimer == 0:
            new_state = GameState()
            new_state.__dict__ = state.__dict__.copy()
            new_state.playerMana -= 173
            new_state.manaSpent += 173
            new_state.poisonTimer = 6
            simulate(new_state, False)
        
        if state.playerMana >= 229 and state.rechargeTimer == 0:
            new_state = GameState()
            new_state.__dict__ = state.__dict__.copy()
            new_state.playerMana -= 229
            new_state.manaSpent += 229
            new_state.rechargeTimer = 5
            simulate(new_state, False)
    
    initial_state.playerHP = 50
    initial_state.playerMana = 500
    simulate(initial_state, True)
    return min_mana

def main():
    with open("input.txt", "r") as file:
        lines = file.readlines()
        bossHP = int(lines[0].split(": ")[1])
        bossDamage = int(lines[1].split(": ")[1])

    initial_state = GameState()
    initial_state.bossHP = bossHP
    initial_state.bossDamage = bossDamage

    print(min_mana_to_win(initial_state))

if __name__ == "__main__":
    main()
