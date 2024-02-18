
fs = require 'fs'
path = require 'path'

# Step 1: Read Input
inputFile = path.join(__dirname, 'input.txt')
inputData = fs.readFileSync(inputFile, 'utf8')
lines = inputData.trim().split('\n')

# Step 2: Create Data Structure
holderMap = {}
heldMap = {}
re = /[a-z]+/g

for line in lines
  names = line.match(re)
  holder = names[0]
  holderMap[holder] = true
  if names.length > 1
    for name in names[1...]
      heldMap[name] = true

# Step 3: Find Root
for holder, _ of holderMap
  if not heldMap[holder]
    console.log holder
    break
