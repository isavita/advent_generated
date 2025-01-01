
fs = require 'fs'

HighCard = 1
OnePair = 2
TwoPair = 3
ThreeKind = 4
FullHouse = 5
FourKind = 6
FiveKind = 7

matches = [[], [], [], [], [], [], []]

findMatches = (hands) ->
  for hand in hands
    count = {}
    for card in hand.cards
      count[card] = (count[card] or 0) + 1
    value = 1
    for c of count
      value *= count[c]
    switch value
      when 1 then matches[6].push hand
      when 2 then matches[5].push hand
      when 3 then matches[3].push hand
      when 4
        if Object.keys(count).length == 2
          matches[1].push hand
        else
          matches[4].push hand
      when 5 then matches[0].push hand
      when 6 then matches[2].push hand

convertAndOrderMatches = () ->
  convertedMatches = []
  for category in matches
    temp = []
    for hand in category
      cards = hand.cards.replace(/A/g, 'E').replace(/T/g, 'A').replace(/J/g, 'B').replace(/Q/g, 'C').replace(/K/g, 'D')
      num = parseInt(cards, 16)
      temp.push {hand: hand, rank: num}
    temp.sort (a, b) -> b.rank - a.rank
    convertedMatches = convertedMatches.concat temp
  convertedMatches

file = fs.readFileSync('input.txt', 'utf-8')
lines = file.trim().split('\n')
hands = []

for line in lines
  [cards, bid] = line.split(' ')
  hands.push {cards: cards, bid: parseInt(bid)}

findMatches(hands)
convertedMatches = convertAndOrderMatches()

total = 0
for i in [0...convertedMatches.length]
  total += convertedMatches[i].hand.bid * (convertedMatches.length - i)

console.log total
