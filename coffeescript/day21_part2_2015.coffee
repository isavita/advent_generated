fs = require 'fs'

class Item
  constructor: (@cost, @damage = 0, @armor = 0) ->

class Character
  constructor: (@hitPoints, @damage = 0, @armor = 0) ->

parseStat = (line) ->
  parts = line.split ': '
  parseInt parts[1]

playerWins = (player, boss) ->
  playerDamage = Math.max 1, player.damage - boss.armor
  bossDamage = Math.max 1, boss.damage - player.armor
  playerTurns = Math.ceil boss.hitPoints / playerDamage
  bossTurns = Math.ceil player.hitPoints / bossDamage
  playerTurns <= bossTurns

data = fs.readFileSync 'input.txt', 'utf8'
lines = data.trim().split '\n'
boss = new Character parseStat(lines[0]), parseStat(lines[1]), parseStat(lines[2])

weapons = [
  new Item 8, 4
  new Item 10, 5
  new Item 25, 6
  new Item 40, 7
  new Item 74, 8
]

armors = [
  new Item 0, 0, 0
  new Item 13, 0, 1
  new Item 31, 0, 2
  new Item 53, 0, 3
  new Item 75, 0, 4
  new Item 102, 0, 5
]

rings = [
  new Item 0
  new Item 25, 1
  new Item 50, 2
  new Item 100, 3
  new Item 20, 0, 1
  new Item 40, 0, 2
  new Item 80, 0, 3
]

maxCost = 0
for w in weapons
  for a in armors
    for ri in [0...rings.length]
      for rj in [ri + 1...rings.length]
        player = new Character 100, w.damage, a.armor
        player.damage += rings[ri].damage + rings[rj].damage
        player.armor += rings[ri].armor + rings[rj].armor
        cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost
        maxCost = Math.max maxCost, cost unless playerWins(player, boss)

console.log maxCost