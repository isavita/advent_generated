
import std/[os, strutils, sequtils, algorithm, re, tables, sets]

type
  Group = object
    units, hp, damage, initiative: int
    attackType: string
    weaknesses, immunities: seq[string]
    isImmune: bool
    id: int

proc getEffectivePower(g: Group): int =
  g.units * g.damage

proc calculateDamage(attacker: Group, defender: Group): int =
  if defender.immunities.contains(attacker.attackType): return 0
  var damage = attacker.getEffectivePower()
  if defender.weaknesses.contains(attacker.attackType): damage *= 2
  return damage

proc parseInput(filename: string): (seq[Group], seq[Group]) =
  var immuneSystem, infection: seq[Group]
  var currentArmy: ptr seq[Group]
  let pattern = re"(\d+) units each with (\d+) hit points (?:\((.*)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)"
  var idCounter = 0
  
  for line in lines(filename):
    if line.strip() == "": continue
    if line.startsWith("Immune System:"):
      currentArmy = addr immuneSystem
    elif line.startsWith("Infection:"):
      currentArmy = addr infection
    else:
      var matches: array[6, string]
      if line.find(pattern, matches) != -1:
        var g: Group
        g.units = matches[0].parseInt
        g.hp = matches[1].parseInt
        g.damage = matches[3].parseInt
        g.attackType = matches[4]
        g.initiative = matches[5].parseInt
        g.isImmune = (currentArmy == addr immuneSystem)
        g.id = idCounter
        idCounter += 1
        
        let extras = matches[2]
        if extras != "":
          for part in extras.split("; "):
            if part.startsWith("weak to "):
              g.weaknesses = part[8..^1].split(", ")
            elif part.startsWith("immune to "):
              g.immunities = part[10..^1].split(", ")
        
        currentArmy[].add(g)
  return (immuneSystem, infection)

proc simulate(immuneIn: seq[Group], infectionIn: seq[Group], boost: int): (int, bool) =
  var immune = immuneIn
  var infection = infectionIn
  for i in 0..<immune.len: immune[i].damage += boost

  while immune.len > 0 and infection.len > 0:
    var targets = initTable[int, int]() # Attacker ID -> Defender Index in allGroups
    var targetedIndices = initHashSet[int]()
    
    var allGroups: seq[Group] = @[]
    for i in 0..<immune.len: allGroups.add(immune[i])
    for i in 0..<infection.len: allGroups.add(infection[i])
    
    # Target Selection Phase
    var selectionOrder = (0..<allGroups.len).toSeq
    selectionOrder.sort(proc(a, b: int): int =
      let epA = allGroups[a].getEffectivePower()
      let epB = allGroups[b].getEffectivePower()
      if epA != epB: return cmp(epB, epA)
      return cmp(allGroups[b].initiative, allGroups[a].initiative)
    )
    
    for i in selectionOrder:
      let attacker = allGroups[i]
      var bestTarget = -1
      var maxDmg = 0
      
      for j in 0..<allGroups.len:
        if allGroups[j].isImmune == attacker.isImmune: continue
        if targetedIndices.contains(j) or allGroups[j].units <= 0: continue
        
        let dmg = calculateDamage(attacker, allGroups[j])
        if dmg == 0: continue
        
        if dmg > maxDmg:
          maxDmg = dmg
          bestTarget = j
        elif dmg == maxDmg and maxDmg > 0:
          let epBest = allGroups[bestTarget].getEffectivePower()
          let epCurr = allGroups[j].getEffectivePower()
          if epCurr > epBest:
            bestTarget = j
          elif epCurr == epBest:
            if allGroups[j].initiative > allGroups[bestTarget].initiative:
              bestTarget = j
              
      if bestTarget != -1:
        targets[attacker.id] = bestTarget
        targetedIndices.incl(bestTarget)

    # Attacking Phase
    var attackOrder = (0..<allGroups.len).toSeq
    attackOrder.sort(proc(a, b: int): int = cmp(allGroups[b].initiative, allGroups[a].initiative))
    
    var totalKilled = 0
    for i in attackOrder:
      if allGroups[i].units <= 0: continue
      if targets.hasKey(allGroups[i].id):
        let targetIdx = targets[allGroups[i].id]
        let dmg = calculateDamage(allGroups[i], allGroups[targetIdx])
        let killed = min(allGroups[targetIdx].units, dmg div allGroups[targetIdx].hp)
        allGroups[targetIdx].units -= killed
        totalKilled += killed
        
    if totalKilled == 0: return (-1, false) # Stalemate

    # Cleanup
    immune = allGroups.filterIt(it.isImmune and it.units > 0)
    infection = allGroups.filterIt(not it.isImmune and it.units > 0)

  let sumImmune = immune.mapIt(it.units).foldl(a + b, 0)
  let sumInfection = infection.mapIt(it.units).foldl(a + b, 0)
  
  if sumImmune > 0: return (sumImmune, true)
  else: return (sumInfection, false)

let (origImmune, origInfection) = parseInput("input.txt")

# Part 1
let (res1, _) = simulate(origImmune, origInfection, 0)
echo "Part One - Winning army units: ", res1

# Part 2
var boost = 1
while true:
  let (remUnits, immuneWon) = simulate(origImmune, origInfection, boost)
  if immuneWon:
    echo "Part Two - Immune system units with boost: ", remUnits
    break
  boost += 1

