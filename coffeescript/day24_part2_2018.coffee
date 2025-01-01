
fs = require 'fs'

contrainsString = (s, v) ->
  for x in s
    return true if x == v
  false

effectivePower = (group) ->
  group.units * group.attackDamage

damageDealt = (attacker, defender) ->
  return 0 if contrainsString(defender.immunities, attacker.attackType)
  if contrainsString(defender.weaknesses, attacker.attackType)
    effectivePower(attacker) * 2
  else
    effectivePower(attacker)

parseGroup = (line) ->
  match = line.match(/^(\d+) units each with (\d+) hit points(?: \((.*?)\))? with an attack that does (\d+) (\w+) damage at initiative (\d+)$/)
  return null unless match
  
  units = parseInt(match[1])
  hitPoints = parseInt(match[2])
  attackDamage = parseInt(match[4])
  attackType = match[5]
  initiative = parseInt(match[6])
  
  immunities = []
  weaknesses = []
  
  if match[3]
    parts = match[3].split('; ')
    for part in parts
      if part.startsWith('immune to ')
        immunities = part.substring('immune to '.length).split(', ')
      else if part.startsWith('weak to ')
        weaknesses = part.substring('weak to '.length).split(', ')
  
  {
    units: units
    hitPoints: hitPoints
    attackDamage: attackDamage
    attackType: attackType
    initiative: initiative
    immunities: immunities
    weaknesses: weaknesses
    attacker: null
    target: null
  }

prepareForBattle = (input) ->
  battle = {}
  initiative = []
  currentArmy = null
  
  for line in input
    if line.endsWith(':')
      currentArmy = line.slice(0, -1)
      battle[currentArmy] = []
    else
      group = parseGroup(line)
      if group
        battle[currentArmy].push(group)
        initiative.push(group)
  
  {battle, initiative}

findTargets = (battle) ->
  for army, groups of battle
    groups.sort (a, b) ->
      if effectivePower(a) > effectivePower(b)
        -1
      else if effectivePower(a) == effectivePower(b)
        b.initiative - a.initiative
      else
        1
    
    for group in groups
      if group.units <= 0
        continue
      
      mostDamage = 0
      targetGroup = null
      
      for enemyArmy, enemyGroups of battle
        if army == enemyArmy
          continue
        
        for enemyGroup in enemyGroups
          if enemyGroup.units <= 0 or enemyGroup.attacker or damageDealt(group, enemyGroup) == 0 or damageDealt(group, enemyGroup) < mostDamage
            continue
          
          if damageDealt(group, enemyGroup) == mostDamage and targetGroup
            if effectivePower(enemyGroup) < effectivePower(targetGroup)
              continue
            if effectivePower(enemyGroup) == effectivePower(targetGroup) and enemyGroup.initiative < targetGroup.initiative
              continue
          
          mostDamage = damageDealt(group, enemyGroup)
          targetGroup = enemyGroup
      
      if targetGroup
        group.target = targetGroup
        targetGroup.attacker = group

attack = (initiative) ->
  initiative.sort (a, b) -> b.initiative - a.initiative
  
  for group in initiative
    if group.units > 0 and group.target and group.target.units > 0
      group.target.units -= Math.floor(damageDealt(group, group.target) / group.target.hitPoints)
    if group.target
      group.target.attacker = null
      group.target = null

clean = (battle, initiative) ->
  for army, groups of battle
    battle[army] = groups.filter (g) -> g.units > 0
  initiative = initiative.filter (g) -> g.units > 0
  initiative.sort (a, b) -> b.initiative - a.initiative
  {battle, initiative}

alive = (army) ->
  for group in army
    return true if group.units > 0
  false

boost = (army, amount) ->
  for group in army
    group.attackDamage += amount

active = (battle) ->
  for army, groups of battle
    return false unless alive(groups)
  true

result = (battle) ->
  winner = null
  units = 0
  for army, groups of battle
    if alive(groups)
      winner = army
      for group in groups
        if group.units > 0
          units += group.units
  {winner, units}

totalUnits = (battle) ->
  sum = 0
  for army, groups of battle
    for group in groups
      if group.units > 0
        sum += group.units
  sum

immuneSystemBoost = (input) ->
  boostAmount = 0
  while true
    {battle, initiative} = prepareForBattle(input)
    boost(battle['Immune System'], boostAmount)
    stalemate = false
    while active(battle)
      before = totalUnits(battle)
      findTargets(battle)
      attack(initiative)
      {battle, initiative} = clean(battle, initiative)
      if totalUnits(battle) == before
        stalemate = true
        break
    if not stalemate
      {winner, units} = result(battle)
      if winner == 'Immune System'
        return units
    boostAmount++

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
console.log immuneSystemBoost(input)
