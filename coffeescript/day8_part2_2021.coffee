
fs = require 'fs'

# Helper function to sort characters in a string
sortString = (str) -> str.split('').sort().join('')

# Function to solve Part One
solvePartOne = (lines) ->
  count = 0
  for line in lines
    [patterns, output] = line.split(' | ')
    outputValues = output.split(' ')
    for value in outputValues
      len = value.length
      if len in [2, 3, 4, 7]  # Lengths for 1, 7, 4, and 8
        count++
  count

# Function to solve Part Two
solvePartTwo = (lines) ->
    totalSum = 0

    for line in lines
        [patterns, output] = line.split(' | ')
        patterns = patterns.split(' ').map(sortString)
        outputValues = output.split(' ').map(sortString)

        # Initialize a map to store the mapping of patterns to digits
        digitMap = {}

        # Find 1, 4, 7, and 8 based on length
        digitMap[1] = patterns.find((p) -> p.length is 2)
        digitMap[4] = patterns.find((p) -> p.length is 4)
        digitMap[7] = patterns.find((p) -> p.length is 3)
        digitMap[8] = patterns.find((p) -> p.length is 7)
       
        # Function to find patterns of a specific length
        findPatternsByLength = (len) -> patterns.filter((p) -> p.length is len)
        
        # Find 6-segment patterns (0, 6, 9)
        sixSegmentPatterns = findPatternsByLength(6)
        digitMap[6] = sixSegmentPatterns.find((p) -> not digitMap[1].split('').every((char) -> p.includes(char)) )
        digitMap[9] = sixSegmentPatterns.find((p) -> digitMap[4].split('').every((char) -> p.includes(char)) )
        digitMap[0] = sixSegmentPatterns.find((p) -> p isnt digitMap[6] and p isnt digitMap[9])
                
        # Find 5-segment patterns (2, 3, 5)
        fiveSegmentPatterns = findPatternsByLength(5)
        digitMap[3] = fiveSegmentPatterns.find((p) -> digitMap[1].split('').every((char) -> p.includes(char)))
        digitMap[5] = fiveSegmentPatterns.find((p) -> p.split('').every((char) -> digitMap[6].includes(char)) )
        digitMap[2] = fiveSegmentPatterns.find((p) -> p isnt digitMap[3] and p isnt digitMap[5])
        
        # Create reverse map
        reverseDigitMap = {}
        for digit, pattern of digitMap
            reverseDigitMap[pattern] = digit

        # Decode output
        decodedOutput = ""
        for value in outputValues
            decodedOutput += reverseDigitMap[value]

        totalSum += parseInt(decodedOutput)
    totalSum


# Read input from file
try
  data = fs.readFileSync('input.txt', 'utf8')
  lines = data.trim().split('\n')

  # Solve and print Part One
  partOneResult = solvePartOne(lines)
  console.log "Part One:", partOneResult

  # Solve and print part Two
  partTwoResult = solvePartTwo(lines)
  console.log "Part Two:", partTwoResult
  
catch err
  console.error 'Error reading file:', err
