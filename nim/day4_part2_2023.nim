
import strutils, sequtils, sets, math

proc parseNumbers(s: string): seq[int] =
  result = newSeq[int]()
  for numStr in s.split(' '):
    let trimmed = numStr.strip()
    if trimmed.len > 0:
      result.add(parseInt(trimmed))

proc calculateMatches(line: string): int =
  # Assumes line format "Card N: W1 W2 ... | G1 G2 ..."
  let parts = line.split(": ")
  let numberParts = parts[1].split(" | ")

  let winningNumbers = parseNumbers(numberParts[0]).toHashSet()
  let givenNumbers = parseNumbers(numberParts[1])

  result = 0
  for givenNum in givenNumbers:
    if winningNumbers.contains(givenNum):
      result.inc

proc main() =
  let inputData = readFile("input.txt").strip()
  # Filter out empty lines that might result from strip() or be in the file
  let lines = inputData.splitLines().filterIt(it.len > 0)

  if lines.len == 0:
    echo 0
    return

  # Calculate the number of matches for each card
  let matches = lines.map(calculateMatches)
  let numCards = matches.len
  
  # Initialize the count of each card to 1
  var counts = newSeqWith(numCards, 1)

  # Process cards to calculate additional copies
  for i in 0 ..< numCards:
    let numMatches = matches[i]
    if numMatches > 0:
      # Get the number of copies of the current card we have
      let currentCardCount = counts[i] 
      # Add copies to subsequent cards
      for j in 1 .. numMatches:
        let nextCardIndex = i + j
        if nextCardIndex < numCards: # Check bounds
          counts[nextCardIndex] += currentCardCount

  # Sum the total number of cards
  echo counts.sum()

main()
