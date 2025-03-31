
import std/[strutils, sequtils, tables, hashes, sets]
import os

const
  InputFile = "input.txt"
  Modulus = 16777216 # 2^24
  NumSteps = 2000
  ChangeSeqLen = 4

# Type alias for the sequence of four price changes
type ChangeSequence = array[ChangeSeqLen, int]

# Calculates the next secret number based on the defined process
proc nextSecret(secret: int): int {.inline.} =
  result = secret # Start with the current secret
  var temp: int

  # Step 1: Multiply by 64, mix (xor), prune (mod)
  temp = result * 64
  result = (result xor temp) mod Modulus

  # Step 2: Divide by 32 (floor), mix, prune
  temp = result div 32 # Integer division rounds down
  result = (result xor temp) mod Modulus

  # Step 3: Multiply by 2048, mix, prune
  temp = result * 2048
  result = (result xor temp) mod Modulus

# Simulates the secret number generation and calculates prices and changes
proc simulateBuyer(initialSecret: int): tuple[prices: seq[int], changes: seq[int]] =
  var prices: seq[int] = newSeq[int](NumSteps + 1) # Initial price + 2000 generated prices
  var changes: seq[int] = newSeq[int](NumSteps)   # 2000 changes
  var currentSecret = initialSecret
  var prevPrice: int

  # Calculate initial price
  prices[0] = currentSecret mod 10
  prevPrice = prices[0]

  # Generate subsequent secrets, prices, and changes
  for i in 1..NumSteps:
    currentSecret = nextSecret(currentSecret)
    let currentPrice = currentSecret mod 10
    prices[i] = currentPrice
    changes[i-1] = currentPrice - prevPrice # Change corresponds to the *new* price
    prevPrice = currentPrice

  return (prices, changes)

# Main execution block
when isMainModule:
  if not fileExists(InputFile):
    echo "Error: Input file '", InputFile, "' not found."
    quit(1)

  # Read initial secret numbers from the file
  let initialSecrets = readFile(InputFile).strip().splitLines().map(parseInt)

  # --- Part 1 Calculation (for reference/verification) ---
  var part1Sum: BiggestInt = 0 # Use BiggestInt for safety, though int might suffice
  for secret in initialSecrets:
    var current = secret
    for _ in 1..NumSteps:
      current = nextSecret(current)
    part1Sum += current
  # echo "Part 1 Sum (Verification): ", part1Sum # You can uncomment this if needed

  # --- Part 2 Calculation ---
  var sequenceScores = initTable[ChangeSequence, BiggestInt]() # Store total bananas per sequence

  for initialSecret in initialSecrets:
    # Simulate this buyer's price history
    let (prices, changes) = simulateBuyer(initialSecret)

    # Ensure we have enough changes to form sequences
    if changes.len < ChangeSeqLen: continue

    # Keep track of sequences already found for *this* buyer to ensure we only score the *first* occurrence
    var foundSequencesPerBuyer = initHashSet[ChangeSequence]()

    # Iterate through all possible starting positions of the 4-change sequence
    # The loop goes up to the last possible start index for a sequence of length ChangeSeqLen
    for i in 0 ..< changes.len - ChangeSeqLen + 1:
      var currentSequence: ChangeSequence
      # Construct the current 4-change sequence
      for j in 0 ..< ChangeSeqLen:
        currentSequence[j] = changes[i + j]

      # Check if we've already found and scored this sequence for *this specific buyer*
      if foundSequencesPerBuyer.containsOrIncl(currentSequence):
          continue # Skip if already found for this buyer

      # If it's the first time seeing this sequence for this buyer:
      # The sale occurs at the price corresponding to the *end* of the sequence.
      # prices[0] is initial.
      # prices[1] is after changes[0].
      # ...
      # prices[i + ChangeSeqLen] is the price after changes[i + ChangeSeqLen - 1] (the last change in the sequence)
      let salePrice = prices[i + ChangeSeqLen]

      # Add the sale price to the total score for this sequence across all buyers
      # Use mgetOrPut to handle adding to existing entries or creating new ones
      sequenceScores.mgetOrPut(currentSequence, 0) += salePrice

  # Find the maximum score among all tested sequences
  var maxBananas: BiggestInt = 0
  if sequenceScores.len > 0: # Check if any sequences were found at all
    for score in sequenceScores.values:
      maxBananas = max(maxBananas, score)
  
  # Print the final answer for Part 2
  echo maxBananas
