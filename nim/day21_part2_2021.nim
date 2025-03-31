
import strutils, sequtils, tables, math, os

# --- Helper Functions ---

proc extractStartingPos(line: string): int =
  # Assumes format "Player X starting position: Y"
  # More robust parsing could be added if needed.
  result = parseInt(line.split(": ")[1])

# --- Part 1 ---

proc solvePart1(p1Start, p2Start: int): BiggestInt =
  ## Simulates the game with the deterministic 100-sided die.
  var
    pos = [p1Start, p2Start]
    score = [0.BiggestInt, 0.BiggestInt]
    dieVal = 1
    totalRolls = 0
    currentPlayer = 0 # 0 for Player 1, 1 for Player 2
  
  # Loop until a player wins
  while score[0] < 1000 and score[1] < 1000:
    var currentRollSum = 0
    # Roll the die 3 times
    for _ in 1..3:
      currentRollSum += dieVal
      dieVal = (dieVal mod 100) + 1 # Cycle 1..100
      totalRolls += 1

    # Update position (1-based indexing, modulo arithmetic)
    pos[currentPlayer] = (pos[currentPlayer] + currentRollSum - 1) mod 10 + 1
    # Update score
    score[currentPlayer] += pos[currentPlayer].BiggestInt

    # Switch player for the next turn
    currentPlayer = 1 - currentPlayer

  # Game ended, determine loser score and calculate result
  let loserScore = if score[0] >= 1000: score[1] else: score[0]
  result = loserScore * totalRolls.BiggestInt

# --- Part 2 ---

# Type definitions for clarity and memoization key
type
  GameState = tuple[p1p: int, p1s: int, p2p: int, p2s: int, turn: int] # pos1, score1, pos2, score2, whose turn (0 or 1)
  WinCounts = tuple[p1w: BiggestInt, p2w: BiggestInt] # Wins for player 1, wins for player 2

# Global memoization table
var memo: Table[GameState, WinCounts]

# Precompute the frequencies of the sums of 3 rolls of a 3-sided die
const rollFrequencies: array[7, tuple[rollSum: int, freq: int]] = [
  (rollSum: 3, freq: 1), # 1+1+1
  (rollSum: 4, freq: 3), # 1+1+2 (3 perms)
  (rollSum: 5, freq: 6), # 1+1+3 (3), 1+2+2 (3)
  (rollSum: 6, freq: 7), # 1+2+3 (6), 2+2+2 (1)
  (rollSum: 7, freq: 6), # 1+3+3 (3), 2+2+3 (3)
  (rollSum: 8, freq: 3), # 2+3+3 (3 perms)
  (rollSum: 9, freq: 1)  # 3+3+3
]
const winScorePart2 = 21

proc countQuantumWins(state: GameState): WinCounts =
  ## Recursively counts wins from a given state using the quantum die, with memoization.
  
  # Base cases: A player has already won
  if state.p1s >= winScorePart2: return (p1w: 1, p2w: 0)
  if state.p2s >= winScorePart2: return (p1w: 0, p2w: 1)

  # Memoization check
  if state in memo: return memo[state]

  var totalWins: WinCounts = (p1w: 0, p2w: 0)

  # Iterate through all possible outcomes of rolling the Dirac die 3 times
  for outcome in rollFrequencies:
    let (rollSum, freq) = outcome
    var nextState = state # Create a mutable copy for the next state

    if state.turn == 0: # Player 1's turn
      nextState.p1p = (state.p1p + rollSum - 1) mod 10 + 1
      nextState.p1s += nextState.p1p
      nextState.turn = 1 # Switch turn
    else: # Player 2's turn
      nextState.p2p = (state.p2p + rollSum - 1) mod 10 + 1
      nextState.p2s += nextState.p2p
      nextState.turn = 0 # Switch turn

    # Recursively call for the next state
    let subWins = countQuantumWins(nextState)

    # Add the wins from the sub-universes, weighted by the frequency of this roll sum
    totalWins.p1w += subWins.p1w * freq.BiggestInt
    totalWins.p2w += subWins.p2w * freq.BiggestInt

  # Store result in memoization table before returning
  memo[state] = totalWins
  result = totalWins

proc solvePart2(p1Start, p2Start: int): BiggestInt =
  ## Initializes and runs the quantum win calculation.
  memo = initTable[GameState, WinCounts]() # Reset memo table for each run (if needed)
  let initialState: GameState = (p1p: p1Start, p1s: 0, p2p: p2Start, p2s: 0, turn: 0)
  let finalWins = countQuantumWins(initialState)
  result = max(finalWins.p1w, finalWins.p2w)

# --- Main Entry Point ---

proc main() =
  # Check if input file exists
  if not fileExists("input.txt"):
    echo "Error: input.txt not found."
    quit(1)
    
  var p1StartPos = 0
  var p2StartPos = 0
  
  try:
    let lines = readFile("input.txt").strip().splitLines()
    if lines.len < 2:
        raise newException(ValueError, "Input file requires at least two lines.")
    p1StartPos = extractStartingPos(lines[0])
    p2StartPos = extractStartingPos(lines[1])
    
    # Validate positions (optional but good practice)
    if p1StartPos notin 1..10 or p2StartPos notin 1..10:
      raise newException(ValueError, "Starting positions must be between 1 and 10.")
      
  except ValueError as e:
    echo "Error parsing input file: ", e.msg
    quit(1)
  except IOError as e:
    echo "Error reading input file: ", e.msg
    quit(1)
  
  # Calculate and print Part 1 result
  let resultPart1 = solvePart1(p1StartPos, p2StartPos)
  echo "Part 1: ", resultPart1

  # Calculate and print Part 2 result
  let resultPart2 = solvePart2(p1StartPos, p2StartPos)
  echo "Part 2: ", resultPart2

# Execute the main procedure only when the script is run directly
when isMainModule:
  main()
