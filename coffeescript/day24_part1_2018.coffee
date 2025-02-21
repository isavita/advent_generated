
fs = require 'fs'

class Group
  constructor: (@army, @id, @units, @hp, @attack, @attackType, @initiative, @weaknesses, @immunities) ->
    @target = null
    @targetedBy = null

  effectivePower: ->
    @units * @attack

  damageTo: (other) ->
    return 0 if other.immunities.includes @attackType
    damage = @effectivePower()
    damage *= 2 if other.weaknesses.includes @attackType
    damage

  attackTarget: ->
    return unless @target? and @units > 0
    damage = @damageTo @target
    killedUnits = Math.min(Math.floor(damage / @target.hp), @target.units) #ensure we don't report killing more units than are left
    @target.units -= killedUnits
    #console.log "#{@army} group #{@id} attacks defending group #{@target.id}, killing #{killedUnits} units"


parseInput = (input) ->
  armies = { 'Immune System': [], 'Infection': [] }
  currentArmy = null
  groupId = 1

  for line in input.trim().split('\n')
    if line.startsWith('Immune System:')
      currentArmy = 'Immune System'
      groupId = 1
    else if line.startsWith('Infection:')
      currentArmy = 'Infection'
      groupId = 1
    else if line.match(/^\d+ units/)
      [_, units, hp, weaknesses_immunities, attack, attackType, initiative] = line.match(/(\d+) units each with (\d+) hit points (.*)with an attack that does (\d+) (\w+) damage at initiative (\d+)/)
      
      weaknesses = []
      immunities = []

      if weaknesses_immunities.length > 0
          weaknesses_immunities = weaknesses_immunities.slice(1,-2) # remove parens
          for part in weaknesses_immunities.split('; ')
              if part.startsWith('weak to')
                  weaknesses = part.slice(8).split(', ')
              else if part.startsWith('immune to')
                  immunities = part.slice(10).split(', ')

      armies[currentArmy].push new Group(currentArmy, groupId++, parseInt(units), parseInt(hp), parseInt(attack), attackType, parseInt(initiative), weaknesses, immunities)
  armies

fight = (armies) ->
    allGroups = [].concat(armies['Immune System'], armies['Infection'])

    # Target selection phase
    for group in allGroups.sort((a, b) ->  #Sorts in decreasing effectivePower, then initiative
      powerDiff = b.effectivePower() - a.effectivePower()
      if powerDiff isnt 0 then powerDiff else b.initiative - a.initiative
      )
      group.target = null
      group.targetedBy = null  #reset for each round

    for group in allGroups.sort((a, b) -> #Sorts in decreasing effectivePower, then initiative
            powerDiff = b.effectivePower() - a.effectivePower()
            if powerDiff isnt 0 then powerDiff else b.initiative - a.initiative
        )

      continue if group.units <= 0  #skip dead units
      targets = []
      enemyArmy = if group.army is 'Immune System' then 'Infection' else 'Immune System'
      
      for potentialTarget in armies[enemyArmy]
          continue if potentialTarget.units <= 0 #dead
          continue if potentialTarget.targetedBy?  #already targeted
          damage = group.damageTo(potentialTarget)
          targets.push({target: potentialTarget, damage: damage}) if damage > 0
          
      targets.sort (a, b) ->  # sort by damage, then effective power, then initative
        damageDiff = b.damage - a.damage
        if damageDiff isnt 0
          return damageDiff
        powerDiff = b.target.effectivePower() - a.target.effectivePower()
        if powerDiff isnt 0
          return powerDiff
        return b.target.initiative - a.target.initiative

      group.target = targets[0]?.target
      group.target?.targetedBy = group if group.target?

    # Attacking phase
    for group in allGroups.sort((a,b) -> b.initiative - a.initiative)
      group.attackTarget()

    #remove empty units
    for armyName of armies
      armies[armyName] = armies[armyName].filter (group) -> group.units > 0

simulateCombat = (armies) ->
  while armies['Immune System'].length > 0 and armies['Infection'].length > 0
    fight armies
    totalUnits = 0
    for armyName of armies
        for group in armies[armyName]
            totalUnits += group.units

  if armies['Immune System'].length > 0
    return armies['Immune System'].reduce(((sum, group) -> sum + group.units), 0)
  else
    return armies['Infection'].reduce(((sum, group) -> sum + group.units), 0)

input = fs.readFileSync('input.txt', 'utf8')
armies = parseInput(input)
winningUnits = simulateCombat(armies)
console.log winningUnits
