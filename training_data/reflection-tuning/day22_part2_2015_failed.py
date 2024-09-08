from collections import namedtuple
import heapq

State = namedtuple('State', ['player_hp', 'player_mana', 'boss_hp', 'shield', 'poison', 'recharge', 'mana_spent'])

def simulate(boss_hp, boss_damage, hard_mode=False):
    initial_state = State(50, 500, boss_hp, 0, 0, 0, 0)
    queue = [(0, initial_state)]
    seen = set()

    while queue:
        _, state = heapq.heappop(queue)

        if state.boss_hp <= 0:
            return state.mana_spent

        if state in seen:
            continue
        seen.add(state)

        # Player turn
        player_hp = state.player_hp - (1 if hard_mode else 0)
        if player_hp <= 0:
            continue

        player_mana = state.player_mana
        boss_hp = state.boss_hp
        shield, poison, recharge = state.shield, state.poison, state.recharge

        # Apply effects
        if poison > 0:
            boss_hp -= 3
        if recharge > 0:
            player_mana += 101

        for spell, cost in [('Magic Missile', 53), ('Drain', 73), ('Shield', 113), ('Poison', 173), ('Recharge', 229)]:
            if player_mana < cost:
                continue

            new_player_hp = player_hp
            new_player_mana = player_mana - cost
            new_boss_hp = boss_hp
            new_shield = max(shield - 1, 0)
            new_poison = max(poison - 1, 0)
            new_recharge = max(recharge - 1, 0)

            if spell == 'Magic Missile':
                new_boss_hp -= 4
            elif spell == 'Drain':
                new_boss_hp -= 2
                new_player_hp += 2
            elif spell == 'Shield':
                if shield > 0:
                    continue
                new_shield = 6
            elif spell == 'Poison':
                if poison > 0:
                    continue
                new_poison = 6
            elif spell == 'Recharge':
                if recharge > 0:
                    continue
                new_recharge = 5

            # Boss turn
            if new_boss_hp > 0:
                if new_poison > 0:
                    new_boss_hp -= 3
                if new_recharge > 0:
                    new_player_mana += 101
                new_player_hp -= max(1, boss_damage - (7 if new_shield > 0 else 0))
                new_shield = max(new_shield - 1, 0)
                new_poison = max(new_poison - 1, 0)
                new_recharge = max(new_recharge - 1, 0)

            if new_player_hp > 0:
                new_state = State(new_player_hp, new_player_mana, new_boss_hp, new_shield, new_poison, new_recharge, state.mana_spent + cost)
                heapq.heappush(queue, (new_state.mana_spent, new_state))

    return float('inf')

# Read boss stats from input
with open('input.txt', 'r') as f:
    boss_hp = int(f.readline().split(': ')[1])
    boss_damage = int(f.readline().split(': ')[1])

# Part 1
print(f"Part 1: {simulate(boss_hp, boss_damage)}")

# Part 2
print(f"Part 2: {simulate(boss_hp, boss_damage, hard_mode=True)}")
