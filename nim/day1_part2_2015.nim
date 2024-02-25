# Function to find the position of the first character that causes Santa to enter the basement
proc findBasementEntryPosition(filename: string): int =
  let file = readFile(filename)
  var floor = 0
  for i, char in file:
    if char == '(':
      inc(floor)
    elif char == ')':
      dec(floor)
    # Check if Santa has reached the basement
    if floor == -1:
      # Return the position, adjusting for 0-indexing
      return i + 1
  return -1

# Main program entry
let filename = "input.txt"
let position = findBasementEntryPosition(filename)
echo "Position: ", position

