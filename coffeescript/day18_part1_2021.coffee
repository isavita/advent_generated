
# Function to parse a snailfish number from a string
parseSnailfishNumber = (str) ->
  JSON.parse(str)

# Function to add two snailfish numbers
addSnailfishNumbers = (num1, num2) ->
  "[" + num1 + "," + num2 + "]"

# Function to explode a snailfish number
explode = (numStr) ->
  depth = 0
  for i in [0...numStr.length]
    if numStr[i] is '['
      depth++
    else if numStr[i] is ']'
      depth--
    else if depth > 4 and numStr[i] is ','
        j = i - 1
        while j >= 0 and numStr[j] isnt '['
          j--
        k = i + 1
        while k < numStr.length and numStr[k] isnt ']'
          k++
        if j >=0 and k < numStr.length
          leftNumStr = numStr.substring(j + 1, i)
          rightNumStr = numStr.substring(i+1, k)
          try
             leftNum = parseInt(leftNumStr)
             rightNum = parseInt(rightNumStr)

             leftPart = numStr.substring(0,j)
             rightPart = numStr.substring(k+1)

             # Add rightNum to first regular number on right
             m = 0
             while m < rightPart.length
                if !isNaN(parseInt(rightPart[m]))
                  n = m
                  while !isNaN(parseInt(rightPart[n])) and n < rightPart.length
                    n++

                  originalRight = parseInt(rightPart.substring(m,n))
                  newRight = originalRight + rightNum
                  rightPart = rightPart.substring(0,m) + newRight + rightPart.substring(n)
                  break;

                m++
            #Add LeftNum to first regular number on the left
             m = leftPart.length - 1
             while m >= 0
                if !isNaN(parseInt(leftPart[m]))
                    n = m
                    while !isNaN(parseInt(leftPart[n])) and n >= 0
                        n--
                    n++

                    originalLeft = parseInt(leftPart.substring(n, m+1))
                    newLeft = originalLeft + leftNum
                    leftPart = leftPart.substring(0, n) + newLeft + leftPart.substring(m+1)
                    break;

                m--
             return leftPart + "0" + rightPart
          catch error
            continue #in case parsing fails


  return numStr

# Function to split a snailfish number
split = (numStr) ->
    match = numStr.match(/(\d{2,})/)
    if match
        num = parseInt(match[0])
        left = Math.floor(num / 2)
        right = Math.ceil(num / 2)
        return numStr.substring(0, match.index) + "[" + left + "," + right + "]" + numStr.substring(match.index + match[0].length)
    return numStr


# Function to reduce a snailfish number
reduce = (numStr) ->
  while true
    exploded = explode(numStr)
    if exploded isnt numStr
      numStr = exploded
      continue

    splitted = split(numStr)
    if splitted isnt numStr
      numStr = splitted
      continue
    break
  return numStr

# Function to calculate the magnitude of a snailfish number
magnitude = (num) ->
  if typeof num is 'number'
    return num
  else
    return 3 * magnitude(num[0]) + 2 * magnitude(num[1])

# Read input from file
fs = require 'fs'
lines = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

# Add all snailfish numbers
sum = lines[0]
for i in [1...lines.length]
  sum = addSnailfishNumbers(sum, lines[i])
  sum = reduce(sum)

# Calculate and print the magnitude of the final sum
finalSum = parseSnailfishNumber(sum)
console.log magnitude(finalSum)
