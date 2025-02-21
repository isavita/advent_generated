
fs = require 'fs'

# Helper function to check if a character is a symbol (not a digit or a period)
isSymbol = (char) ->
  char? and not char.match(/[0-9.]/)

# Helper function to extract the full number, given a starting position
extractNumber = (line, col) ->
  return null unless line[col]?.match(/[0-9]/)
  start = col
  end = col
  start-- while start >= 0 and line[start].match(/[0-9]/)
  end++ while end < line.length and line[end].match(/[0-9]/)
  parseInt(line.substring(start + 1, end))

# Reads the input from the file
lines = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

# Part 1: Sum of part numbers
sumOfPartNumbers = 0
# Part 2: Sum of gear ratios
sumOfGearRatios = 0

for row in [0...lines.length]
  for col in [0...lines[row].length]
    #part 1 logic
    if isSymbol(lines[row][col])
      numbersFound = {} # Use a set (object as keys) to avoid duplicates

      for dx in [-1..1]
        for dy in [-1..1]
          continue if dx is 0 and dy is 0 # Skip the symbol itself

          newRow = row + dx
          newCol = col + dy

          if newRow >= 0 and newRow < lines.length and newCol >= 0 and newCol < lines[newRow].length
            num = extractNumber(lines[newRow], newCol)
            numbersFound[num] = true if num?  # Add number to the set

      for num of numbersFound
        sumOfPartNumbers += num

    #Part 2 logic
    if lines[row][col] == '*'
       numbersFound = {}

       for dx in [-1..1]
         for dy in [-1..1]
           continue if dx is 0 and dy is 0

           newRow = row + dx
           newCol = col + dy
           if newRow >= 0 and newRow < lines.length and newCol >= 0 and newCol < lines[newRow].length
                num = extractNumber(lines[newRow], newCol)
                numbersFound[num] = true if num?

       if Object.keys(numbersFound).length == 2
            gearRatio = 1
            for num of numbersFound
                gearRatio *= num
            sumOfGearRatios+= gearRatio

# Output the results
console.log sumOfPartNumbers
console.log sumOfGearRatios
