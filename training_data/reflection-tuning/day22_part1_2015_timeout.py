import sys
from functools import lru_cache

class State:
    def __init__(self, player_hp, player_mana, boss_hp, boss_damage, shield=0, poison=0, recharge=0, mana_spent=0):
        self.player_hp = player_hp
        self.player_mana = player_mana
        self.boss_hp = boss_hp
        self.boss_damage = boss_damage
        self.shield = shield
        self.poison = poison
        self.recharge = recharge
        self.mana_spent = mana_spent

    def __hash__(self):
        return hash((self.player_hp, self.player_mana, self.boss_hp, self.shield, self.poison, self.recharge))

SPELLS = [
    ("Magic Missile", 53, 4, 0),
    ("Drain", 73, 2, 2),
    ("Shield", 113, 0, 0),
    ("Poison", 173, 0, 0),
    ("Recharge", 229, 0, 0)
]

@lru_cache(maxsize=None)
def simulate_turn(state, spell):
    new_state = State(state.player_hp, state.player_mana, state.boss_hp, state.boss_damage,
                      state.shield, state.poison, state.recharge, state.mana_spent)
    
    # Apply effects
    if new_state.shield > 0:
        new_state.shield -= 1
    if new_state.poison > 0:
        new_state.boss_hp -= 3
        new_state.poison -= 1
    if new_state.recharge > 0:
        new_state.player_mana += 101
        new_state.recharge -= 1
    
    # Cast spell
    new_state.player_mana -= spell[1]
    new_state.mana_spent += spell[1]
    new_state.boss_hp -= spell[2]
    new_state.player_hp += spell[3]
    
    if spell[0] == "Shield":
        new_state.shield = 6
    elif spell[0] == "Poison":
        new_state.poison = 6
    elif spell[0] == "Recharge":
        new_state.recharge = 5
    
    # Boss turn
    if new_state.boss_hp > 0:
        damage = new_state.boss_damage - (7 if new_state.shield > 0 else 0)
        new_state.player_hp -= max(1, damage)
    
    return new_state

def dfs(state, min_mana):
    if state.mana_spent >= min_mana[0] or state.player_hp <= 0:
        return float('inf')
    
    if state.boss_hp <= 0:
        min_mana[0] = min(min_mana[0], state.mana_spent)
        return state.mana_spent
    
    best_mana = float('inf')
    for spell in SPELLS:
        if spell[1] <= state.player_mana and (
            (spell[0] != "Shield" or state.shield == 0) and
            (spell[0] != "Poison" or state.poison == 0) and
            (spell[0] != "Recharge" or state.recharge == 0)
        ):
            new_state = simulate_turn(state, spell)
            result = dfs(new_state, min_mana)
            best_mana = min(best_mana, result)
    
    return best_mana

def solve(boss_hp, boss_damage):
    initial_state = State(50, 500, boss_hp, boss_damage)
    min_mana = [float('inf')]
    result = dfs(initial_state, min_mana)
    return result

# Read input from file
with open('input.txt', 'r') as f:
    boss_hp = int(f.readline().split(': ')[1])
    boss_damage = int(f.readline().split(': ')[1])

# Solve and print the result
print(solve(boss_hp, boss_damage))
