
fs = require 'fs'

valueDict = { 'J': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9, 'T': 10, 'Q': 11, 'K': 12, 'A': 13 }

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')

hands = []
for line in input
  [cards, bid] = line.split(' ')
  hands.push { cards, bid: parseInt(bid) }

matches = (for i in [0...7] then [])

for hand in hands
  count = {}
  for card in hand.cards
    count[card] = (count[card] or 0) + 1

  if count['J']? and count['J'] > 0
    highV = 0
    highKey = 'J'
    for key, value of count
      if key != 'J'
        if value > highV
          highKey = key
          highV = value
        else if value == highV and valueDict[key] > valueDict[highKey]
          highKey = key
    if highKey != 'J'
      count[highKey] += count['J']
      delete count['J']

  value = 1
  for _, v of count
    value *= v

  switch value
    when 1 then matches[6].push hand
    when 2 then matches[5].push hand
    when 3 then matches[3].push hand
    when 4
      if Object.keys(count).length == 2 then matches[1].push hand else matches[4].push hand
    when 5 then matches[0].push hand
    when 6 then matches[2].push hand

convertedMatches = []

for x in matches
  temp = []
  for hand in x
    y = hand.cards.replace(/A/g, 'E').replace(/T/g, 'A').replace(/J/g, '1').replace(/Q/g, 'C').replace(/K/g, 'D')
    val = parseInt(y, 16)
    temp.push [val, hand.bid]
  temp.sort (a, b) -> b[0] - a[0]
  convertedMatches = convertedMatches.concat temp

total = 0
for x in [0...convertedMatches.length]
  total += convertedMatches[x][1] * (convertedMatches.length - x)

console.log total
