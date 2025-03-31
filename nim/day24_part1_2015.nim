
import std/[strutils, sequtils, algorithm, math, sets, os]

# --- Helper Functions ---

# Calculates the Quantum Entanglement (product of weights)
proc calculateQE(group: openArray[int]): BiggestInt =
  result = 1 # Use BiggestInt to prevent overflow
  for weight in group:
    result *= weight

# Recursive helper function to check if a subset summing to target exists
# Uses backtracking.
proc hasSubsetSum(items: openArray[int], target: int, index: int, currentSum: int): bool =
  # Base cases
  if currentSum == target:
    return true
  # Pruning: cannot reach target or went past end
  if currentSum > target or index >= items.len:
    return false

  # Recursive steps:
  # 1. Try including the current item
  if hasSubsetSum(items, target, index + 1, currentSum + items[index]):
    return true

  # 2. Try excluding the current item
  if hasSubsetSum(items, target, index + 1, currentSum):
    return true

  # No subset found from this path
  return false

# Recursive function to find combinations for the first group
# It finds combinations of exactly size 'k' that sum to 'targetWeight'.
# For each valid combination found, it checks if the remaining items can form the other groups.
# Updates minQE if a valid configuration with better QE is found for the current size k.
proc findCombinations(
    weights: openArray[int],   # All package weights, sorted descending
    targetWeight: int,         # The target weight for each group
    k: int,                    # The target size for the first group
    minQE: var BiggestInt,     # The minimum QE found so far for size k (updated by ref)
    
    # --- Recursion state ---
    idx: int,                  # Current index in weights being considered
    currentSum: int,           # Sum of weights currently in the group
    currentGroupIndices: var seq[int] # Indices of items included so far
) =
  
  # --- Pruning ---
  # 1. Exceeded target sum or size k
  if currentSum > targetWeight or currentGroupIndices.len > k:
    return

  # 2. Not enough remaining items to reach size k
  if currentGroupIndices.len + (weights.len - idx) < k:
      return

  # 3. Sum is too low even if we add all remaining items (optimization)
  var remainingSumPossible = 0
  for i in idx..<weights.len: remainingSumPossible += weights[i]
  if currentSum + remainingSumPossible < targetWeight:
      return


  # --- Base Case: Found a potential Group 1 ---
  if currentSum == targetWeight:
    if currentGroupIndices.len == k:
      # --- Valid Group 1 found: Check if remaining packages can be split ---

      # 1. Construct the actual group 1 weights and the remaining weights
      var group1 = newSeq[int](k)
      var remainingWeights = newSeq[int]()
      var used = newSeq[bool](weights.len) # Track used indices efficiently
      for i, groupIdx in currentGroupIndices:
        group1[i] = weights[groupIdx]
        used[groupIdx] = true
      
      for i, w in weights:
        if not used[i]:
          remainingWeights.add(w)

      # 2. Check if remainingWeights can be partitioned into two groups of targetWeight.
      #    We only need to check if *one* subset sums to targetWeight. If it does,
      #    the rest *must* also sum to targetWeight because the total sum is 3 * targetWeight.
      if hasSubsetSum(remainingWeights, targetWeight, 0, 0):
        # This is a valid configuration! Calculate QE and update minQE if better.
        let qe = calculateQE(group1)
        minQE = min(minQE, qe)
        
    # Whether valid or not (size might not be k), we stop this path as sum is met
    return 

  # --- Base Case: Cannot continue search ---
  if idx >= weights.len:
    return

  # --- Recursive Steps ---

  # 1. Include weights[idx] in the group
  currentGroupIndices.add(idx)
  findCombinations(weights, targetWeight, k, minQE, idx + 1, currentSum + weights[idx], currentGroupIndices)
  currentGroupIndices.setLen(currentGroupIndices.len - 1) # Backtrack

  # 2. Exclude weights[idx] from the group
  #    Optimization: Only explore excluding if enough remaining items exist to reach size k
  if weights.len - (idx + 1) >= k - currentGroupIndices.len:
    findCombinations(weights, targetWeight, k, minQE, idx + 1, currentSum, currentGroupIndices)


# --- Main Solver ---

proc solve(filename: string): BiggestInt =
  let lines = readFile(filename).strip.splitLines
  var weights = lines.map(parseInt)

  # Sort weights descending - helps find solutions with fewer items earlier
  # and potentially prune the search space faster in findCombinations.
  weights.sort(Descending)

  let totalWeight = weights.sum()
  if totalWeight mod 3 != 0:
    raise newException(ValueError, "Total weight is not divisible by 3")
  
  let targetWeight = totalWeight div 3

  # Iterate through possible sizes (k) for the first group, starting from the smallest.
  for k in 1..weights.len: 
    var minQEForSizeK = BiggestInt.high # Reset min QE for this specific size k
    var currentIndices = newSeq[int]() # Holds indices for the current combination search

    # Find all valid configurations where group 1 has size k
    findCombinations(weights, targetWeight, k, minQEForSizeK, 0, 0, currentIndices)

    # If we found *any* valid configuration for this size k,
    # then this k is the minimum possible size. We return the best QE found for this k.
    if minQEForSizeK != BiggestInt.high:
      return minQEForSizeK

  # Should be unreachable if a solution exists as per problem description
  raise newException(ValueError, "No valid configuration found")

# --- Entry Point ---
when isMainModule:
  let inputFilename = "input.txt"
  if not fileExists(inputFilename):
    echo "Error: Input file '", inputFilename, "' not found."
  else:
    try:
      let result = solve(inputFilename)
      echo result
    except ValueError as e:
      echo "Error: ", e.msg
    except Exception as e:
      echo "An unexpected error occurred: ", e.msg

