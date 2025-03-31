
import os
import strutils
import math # Potentially needed for standard powm if used, but we implement our own.
import parseutils

# --- Constants ---

const DECK_SIZE1: int64 = 10007
const CARD_TO_FIND1: int64 = 2019

const DECK_SIZE2: int64 = 119315717514047
const SHUFFLE_COUNT2: int64 = 101741582076661
const POS_TO_FIND2: int64 = 2020

# --- Modular Arithmetic for Large Integers ---
# Nim's int64 is sufficient for the moduli and counts, but intermediate
# multiplications a*b might exceed 2^63-1 before the modulo operation.
# We need safe modular arithmetic functions.

proc addm(a, b, m: int64): int64 =
  # Calculate (a + b) mod m safely.
  # Since a, b < m, a + b < 2m. If 2m fits within int64, standard add is safe.
  # 2 * 1.2e14 is approx 2.4e14, which fits in int64 (max ~9e18).
  # However, ensure result is always non-negative.
  result = (a + b) mod m
  if result < 0:
    result += m

proc subm(a, b, m: int64): int64 =
  # Calculate (a - b) mod m safely, ensuring non-negative result.
  result = (a - b) mod m
  if result < 0:
    result += m

proc mulm(a, b, m: int64): int64 =
  # Calculate (a * b) mod m safely using Russian Peasant Multiplication (binary exponentiation).
  # This avoids intermediate overflow if a * b > high(int64).
  var res: int64 = 0
  var aa = a mod m # Reduce operands first
  var bb = b mod m
  if aa < 0: aa += m
  if bb < 0: bb += m

  while bb > 0:
    if (bb and 1) == 1: # If current bit of b is 1
      res = addm(res, aa, m) # Add aa to result (safely)
    aa = addm(aa, aa, m)   # Double aa (safely) equivalent to (aa * 2) mod m
    bb = bb shr 1          # Move to next bit of b
  return res

proc powm_safe(base, exp, m: int64): int64 =
  # Modular exponentiation base^exp mod m using safe multiplication (mulm).
  var res: int64 = 1
  var b = base mod m
  var e = exp
  if b < 0: b += m

  while e > 0:
    if (e and 1) == 1:
      res = mulm(res, b, m) # Multiply result by base (safely)
    b = mulm(b, b, m)     # Square the base (safely)
    e = e shr 1           # Move to next bit of exponent
  return res

proc modInverse(n, m: int64): int64 =
  # Modular multiplicative inverse of n modulo m.
  # Assumes m is prime (using Fermat's Little Theorem: n^(m-2) mod m).
  # The modulus DECK_SIZE2 is prime.
  if m <= 1: raise newException(ValueError, "Modulus must be > 1")
  let n_reduced = n mod m
  if n_reduced == 0: raise newException(ValueError, "Cannot compute inverse of 0 mod m")
  # Exponent is m-2
  return powm_safe(n_reduced, m - 2, m)

# --- Shuffle Logic as Linear Transformation ---

# Represents a shuffle operation f(p) as p' = (a*p + b) mod M
type ShuffleTransform = tuple[a, b: int64]

proc parseInstruction(line: string, m: int64): ShuffleTransform =
  # Parses a shuffle instruction line and returns the (a, b) transformation.
  if line == "deal into new stack":
    # p' = M - 1 - p = -p + (M - 1) = (-1 * p - 1) mod M
    result = (subm(0, 1, m), subm(0, 1, m)) # a = -1, b = -1 (mod m)
  elif line.startsWith("cut"):
    # p' = (p - N) mod M = (1 * p - N) mod M
    let n = parseInt(line.split(' ')[^1])
    result = (1, subm(0, n, m)) # a = 1, b = -N (mod m)
  elif line.startsWith("deal with increment"):
    # p' = (p * N) mod M = (N * p + 0) mod M
    let n = parseInt(line.split(' ')[^1])
    result = (n mod m, 0) # a = N (mod m), b = 0
  else:
    raise newException(ValueError, "Invalid instruction: " & line)

proc combine(t1: ShuffleTransform, t2: ShuffleTransform, m: int64): ShuffleTransform =
  # Combines two transformations t1 and t2, where t2 is applied AFTER t1.
  # t1: p'  = a1*p + b1
  # t2: p'' = a2*p' + b2
  # Combined: p'' = a2*(a1*p + b1) + b2 = (a2*a1)*p + (a2*b1 + b2)
  let (a1, b1) = t1
  let (a2, b2) = t2
  let new_a = mulm(a2, a1, m)
  let term_b = mulm(a2, b1, m)
  let new_b = addm(term_b, b2, m)
  result = (new_a, new_b)

# --- Main Program ---

proc main() =
  let filename = "input.txt"
  if not fileExists(filename):
    echo "Error: input.txt not found."
    quit(1)

  let instructions = readFile(filename).strip().splitLines()

  # --- Part 1 ---
  block part1:
    let m1 = DECK_SIZE1
    # Start with identity transformation: p' = 1*p + 0
    var transform: ShuffleTransform = (1, 0)

    # Combine all instructions into a single transformation
    for line in instructions:
      if line.len == 0: continue
      let instructionTransform = parseInstruction(line, m1)
      transform = combine(transform, instructionTransform, m1)

    # Apply the final transformation to the initial position of the card
    let (a, b) = transform
    let initial_pos: int64 = CARD_TO_FIND1
    # final_pos = (a * initial_pos + b) mod m1
    let final_pos = addm(mulm(a, initial_pos, m1), b, m1)
    echo "Part 1: ", final_pos

  # --- Part 2 ---
  block part2:
    let m2 = DECK_SIZE2
    let k = SHUFFLE_COUNT2
    let target_pos: int64 = POS_TO_FIND2

    # Calculate the combined transformation (a, b) for one full shuffle sequence
    var single_shuffle_transform: ShuffleTransform = (1, 0)
    for line in instructions:
      if line.len == 0: continue
      let instructionTransform = parseInstruction(line, m2)
      single_shuffle_transform = combine(single_shuffle_transform, instructionTransform, m2)

    let (a, b) = single_shuffle_transform

    # Calculate the transformation (A_K, B_K) for K shuffles
    # f^K(p) = A_K * p + B_K (mod m2)
    # A_K = a^K mod m2
    # B_K = b * (a^K - 1) * modInverse(a - 1, m2) mod m2  (if a != 1)
    # B_K = b * K mod m2                                (if a == 1)

    var a_k: int64
    var b_k: int64

    if a == 1:
      a_k = 1
      b_k = mulm(b, k, m2) # Transformation is p' = p + K*b
    else:
      a_k = powm_safe(a, k, m2)               # a^K mod m2
      let term1 = subm(a_k, 1, m2)             # (a^K - 1) mod m2
      let a_minus_1 = subm(a, 1, m2)
      let term2 = modInverse(a_minus_1, m2)    # (a - 1)^(-1) mod m2
      let geometric_sum_factor = mulm(term1, term2, m2) # (a^K - 1)/(a-1) mod m2
      b_k = mulm(b, geometric_sum_factor, m2)  # b * Sum(a^i for i=0..K-1) mod m2

    # We have the final transformation after K shuffles: f^K(p) = A_K * p + B_K
    # We know the final position (target_pos) and want the initial position (card number).
    # target_pos = (A_K * initial_pos + B_K) mod m2
    # Rearrange to find initial_pos:
    # initial_pos = (target_pos - B_K) * modInverse(A_K, m2) mod m2

    let term_lhs = subm(target_pos, b_k, m2) # (target_pos - B_K) mod m2
    let a_k_inv = modInverse(a_k, m2)       # A_K^(-1) mod m2
    let initial_pos = mulm(term_lhs, a_k_inv, m2)

    echo "Part 2: ", initial_pos

# --- Entry Point ---
when isMainModule:
  main()
