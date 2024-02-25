
import os
import strutils

type Item = object
    cost: int
    damage: int
    armor: int

type Character = object
    hitPoints: int
    damage: int
    armor: int

proc parseStat(line: string): int =
    result = parseInt(split(line, ": ")[1])

proc playerWins(player: Character, boss: Character): bool =
    proc max(a, b: int): int =
        if a > b: return a
        return b

    var playerDamage = max(1, player.damage - boss.armor)
    var bossDamage = max(1, boss.damage - player.armor)

    var playerTurns = (boss.hitPoints + playerDamage - 1) div playerDamage
    var bossTurns = (player.hitPoints + bossDamage - 1) div bossDamage

    result = playerTurns <= bossTurns

var data = readFile("input.txt")
var lines = split(data, "\n")
var boss = Character(hitPoints: parseStat(lines[0]), damage: parseStat(lines[1]), armor: parseStat(lines[2]))

var weapons = [Item(cost: 8, damage: 4), Item(cost: 10, damage: 5), Item(cost: 25, damage: 6), Item(cost: 40, damage: 7), Item(cost: 74, damage: 8)]

var armors = [Item(cost: 0, armor: 0), Item(cost: 13, armor: 1), Item(cost: 31, armor: 2), Item(cost: 53, armor: 3), Item(cost: 75, armor: 4), Item(cost: 102, armor: 5)]

var rings = [Item(cost: 0), Item(cost: 25, damage: 1), Item(cost: 50, damage: 2), Item(cost: 100, damage: 3), Item(cost: 20, armor: 1), Item(cost: 40, armor: 2), Item(cost: 80, armor: 3)]

var minCost = high(int)
for w in weapons:
    for a in armors:
        for ri in 0 ..< len(rings):
            for rj in ri + 1 ..< len(rings):
                var player = Character(hitPoints: 100, damage: w.damage, armor: a.armor)
                player.damage += rings[ri].damage + rings[rj].damage
                player.armor += rings[ri].armor + rings[rj].armor
                var cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost
                if playerWins(player, boss) and cost < minCost:
                    minCost = cost

echo minCost
