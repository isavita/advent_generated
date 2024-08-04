fs = require 'fs'

fileContent = fs.readFileSync('input.txt', 'utf8')
lines = fileContent.split('\n')

replacements = []
molecule = ''

for line in lines
  if line == ''
    continue
  if line.includes(' => ')
    replacements.push(line)
  else
    molecule = line

molecules = {}

for replacement in replacements
  [from, to] = replacement.split(' => ')
  for i in [0...molecule.length]
    if molecule.substr(i, from.length) == from
      newMolecule = molecule.substr(0, i) + to + molecule.substr(i + from.length)
      molecules[newMolecule] = true

console.log(Object.keys(molecules).length)