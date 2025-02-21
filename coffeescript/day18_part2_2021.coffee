
fs = require 'fs'

# Parse a snailfish number from a string
parseSnailfishNumber = (str) -> JSON.parse(str)

# Convert a snailfish number back to a string (for debugging)
snailfishToString = (num) -> JSON.stringify(num)

# Add two snailfish numbers
addSnailfishNumbers = (num1, num2) ->
  [num1, num2]

# Explode a snailfish number (if needed)
explode = (num, depth = 0) ->
    if typeof num is 'number'
        return [false, num]

    [left, right] = num

    if depth >= 4
      return [true, 0, left, right]
    
    [exploded, newLeft, leftCarry, rightCarry] = explode(left, depth + 1)
    if exploded
        if rightCarry > 0
            addRight = (n, val) ->
                if typeof n is 'number'
                    n + val
                else
                    [addRight(n[0], val), n[1]]
            right = addRight(right, rightCarry)

        return [true, [newLeft, right], leftCarry, 0]
        
    [exploded, newRight, leftCarry, rightCarry] = explode(right, depth + 1)
    if exploded
        if leftCarry > 0
            addLeft = (n, val) ->
                if typeof n is 'number'
                    n + val
                else
                    [n[0], addLeft(n[1], val)]
            left = addLeft(left, leftCarry)
        return [true, [left, newRight], 0, rightCarry]

    return [false, num]
        

# Split a snailfish number (if needed)
split = (num) ->
  if typeof num is 'number'
    if num >= 10
      return [true, [Math.floor(num / 2), Math.ceil(num / 2)]]
    else
      return [false, num]

  [left, right] = num
  [splitLeft, newLeft] = split(left)
  if splitLeft
    return [true, [newLeft, right]]
  
  [splitRight, newRight] = split(right)
  if splitRight
    return [true, [left, newRight]]

  return [false, num]
    

# Reduce a snailfish number
reduce = (num) ->
  loop
    [exploded, newNum, _, _] = explode(num)
    if exploded
      num = newNum
      continue

    [splitResult, newNum] = split(num)
    if splitResult
      num = newNum
      continue
      
    break
  num

# Calculate the magnitude of a snailfish number
magnitude = (num) ->
  if typeof num is 'number'
    num
  else
    3 * magnitude(num[0]) + 2 * magnitude(num[1])

# Read input from file
lines = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
numbers = (parseSnailfishNumber(line) for line in lines)

# Part 1: Calculate the magnitude of the sum of all numbers
sum = numbers[0]
for num in numbers[1..]
  sum = addSnailfishNumbers(sum, num)
  sum = reduce(sum)
console.log "Part 1:", magnitude(sum)


# Part 2: Find the largest magnitude from adding two numbers
maxMagnitude = 0
for i in [0...numbers.length]
  for j in [0...numbers.length]
    continue if i is j
    
    sum1 = reduce(addSnailfishNumbers(numbers[i], numbers[j]))
    mag1 = magnitude(sum1)
    maxMagnitude = Math.max(maxMagnitude, mag1)

    sum2 = reduce(addSnailfishNumbers(numbers[j], numbers[i]))
    mag2 = magnitude(sum2)
    maxMagnitude = Math.max(maxMagnitude, mag2)

console.log "Part 2:", maxMagnitude
