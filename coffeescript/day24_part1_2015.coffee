fs = require 'fs'

data = fs.readFileSync('input.txt', 'utf8')
lines = data.trim().split '\n'
packages = []
totalWeight = 0

for line in lines
  weight = parseInt line
  packages.push weight
  totalWeight += weight

targetWeight = totalWeight / 3
bestQE = Infinity
bestLength = Infinity

for comb in [1...1 << packages.length]
  groupWeight = 0
  qe = 1
  groupLength = 0

  for i in [0...packages.length]
    if comb & (1 << i)
      groupWeight += packages[i]
      qe *= packages[i]
      groupLength += 1

  if groupWeight == targetWeight and groupLength <= bestLength
    if groupLength < bestLength or qe < bestQE
      bestLength = groupLength
      bestQE = qe

console.log bestQE