fs = require 'fs'

data = fs.readFileSync 'input.txt', 'utf8'
lines = data.trim().split '\n'
packages = (parseInt line for line in lines)
totalWeight = packages.reduce (a, b) -> a + b

targetWeight = Math.floor totalWeight / 4
bestQE = Number.MAX_SAFE_INTEGER
bestLength = Number.MAX_SAFE_INTEGER

canSplit = (packages, firstGroupComb, targetWeight) ->
  remainingPackages = (weight for weight, i in packages when not (firstGroupComb & (1 << i)))
  for comb1 in [1...(1 << remainingPackages.length)]
    group1Weight = 0
    for i in [0...remainingPackages.length]
      group1Weight += remainingPackages[i] if comb1 & (1 << i)
    if group1Weight == targetWeight
      for comb2 in [1...(1 << remainingPackages.length)]
        if not (comb1 & comb2)
          group2Weight = 0
          for i in [0...remainingPackages.length]
            group2Weight += remainingPackages[i] if comb2 & (1 << i)
          return true if group2Weight == targetWeight
  false

for comb in [1...(1 << packages.length)]
  groupWeight = 0
  qe = 1
  groupLength = 0
  for i in [0...packages.length]
    if comb & (1 << i)
      groupWeight += packages[i]
      qe *= packages[i]
      groupLength++
  if groupWeight == targetWeight and groupLength <= bestLength
    if groupLength < bestLength or qe < bestQE
      if canSplit packages, comb, targetWeight
        bestLength = groupLength
        bestQE = qe

console.log bestQE