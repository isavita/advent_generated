
import sys

class Item:
    def __init__(self, cost=0, damage=0, armor=0):
        self.cost = cost
        self.damage = damage
        self.armor = armor

class Character:
    def __init__(self, hit_points=0, damage=0, armor=0):
        self.hit_points = hit_points
        self.damage = damage
        self.armor = armor

def parse_stat(line):
    return int(line.split(": ")[1])

def player_wins(player, boss):
    player_damage = max(1, player.damage - boss.armor)
    boss_damage = max(1, boss.damage - player.armor)

    player_turns = (boss.hit_points + player_damage - 1) // player_damage
    boss_turns = (player.hit_points + boss_damage - 1) // boss_damage

    return player_turns <= boss_turns

def max(a, b):
    return a if a > b else b

data = open("input.txt", "r")
lines = data.readlines()
data.close()

boss = Character(hit_points=parse_stat(lines[0]), damage=parse_stat(lines[1]), armor=parse_stat(lines[2]))

weapons = [
    Item(cost=8, damage=4),
    Item(cost=10, damage=5),
    Item(cost=25, damage=6),
    Item(cost=40, damage=7),
    Item(cost=74, damage=8)
]

armors = [
    Item(),
    Item(cost=13, armor=1),
    Item(cost=31, armor=2),
    Item(cost=53, armor=3),
    Item(cost=75, armor=4),
    Item(cost=102, armor=5)
]

rings = [
    Item(),
    Item(cost=25, damage=1),
    Item(cost=50, damage=2),
    Item(cost=100, damage=3),
    Item(cost=20, armor=1),
    Item(cost=40, armor=2),
    Item(cost=80, armor=3)
]

max_cost = 0
for w in weapons:
    for a in armors:
        for ri in range(len(rings)):
            for rj in range(ri + 1, len(rings)):
                player = Character(hit_points=100, damage=w.damage, armor=a.armor)
                player.damage += rings[ri].damage + rings[rj].damage
                player.armor += rings[ri].armor + rings[rj].armor
                cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost
                if not player_wins(player, boss) and cost > max_cost:
                    max_cost = cost

print(max_cost)
