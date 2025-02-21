
fs = require 'fs'

# Function to calculate points for a single card (Part 1)
calculatePoints = (winningNumbers, myNumbers) ->
  matches = myNumbers.filter (n) -> winningNumbers.includes n
  if matches.length > 0
    return Math.pow(2, matches.length - 1)
  else
    return 0

# Function to count matching numbers (used for both parts)
countMatches = (winningNumbers, myNumbers) ->
    myNumbers.filter((n) -> winningNumbers.includes n).length

# Read the input file
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.error "Error reading file:", err
    return

  lines = data.trim().split('\n')

  # Part 1: Calculate total points
  totalPoints = 0
  for line in lines
    [cardPart, numbersPart] = line.split(': ')
    [winningNumbersStr, myNumbersStr] = numbersPart.split(' | ')
    winningNumbers = winningNumbersStr.trim().split(/\s+/).map(Number)
    myNumbers = myNumbersStr.trim().split(/\s+/).map(Number)
    totalPoints += calculatePoints(winningNumbers, myNumbers)

  console.log "Part 1 - Total Points:", totalPoints

  # Part 2:  Count total scratchcards including copies
  cardCounts = (1 for _ in lines) # Initialize with 1 of each original card

  for i in [0...lines.length]
      line = lines[i]
      [cardPart, numbersPart] = line.split(':')
      [winningNumbersStr, myNumbersStr] = numbersPart.split('|')
      winningNumbers = winningNumbersStr.trim().split(/\s+/).map(Number)
      myNumbers = myNumbersStr.trim().split(/\s+/).map(Number)

      matches = countMatches(winningNumbers, myNumbers)

      # Win copies of subsequent cards
      for j in [i + 1 ... Math.min(i + 1 + matches, lines.length)]
          cardCounts[j] += cardCounts[i]

  totalScratchcards = cardCounts.reduce ((acc, val) -> acc + val), 0
  console.log "Part 2 - Total Scratchcards:", totalScratchcards
