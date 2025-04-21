
# Import necessary modules
fs = require 'fs'

# --- BigInt Modular Arithmetic Helpers ---

# Modular exponentiation (base^exponent % modulus)
# Handles BigInt inputs
modPow = (base, exponent, modulus) ->
  # Ensure all inputs are BigInt
  base = BigInt(base)
  exponent = BigInt(exponent)
  modulus = BigInt(modulus)

  if modulus == 1n then return 0n

  result = 1n
  base = base % modulus # Reduce base initially

  while exponent > 0n
    # If exponent is odd, multiply base with result
    if exponent % 2n == 1n
      result = (result * base) % modulus
    # Exponent must be even now
    exponent = exponent >> 1n # exponent = exponent / 2
    base = (base * base) % modulus # base = base^2 % modulus
  
  # Ensure result is non-negative
  (result + modulus) % modulus

# Modular multiplicative inverse using Fermat's Little Theorem
# a^(m-2) % m is the inverse of a mod m, if m is prime
# Handles BigInt inputs
modInverse = (n, modulus) ->
  n = BigInt(n)
  modulus = BigInt(modulus)
  
  # Calculate inverse using modular exponentiation: n^(modulus-2) % modulus
  inv = modPow(n, modulus - 2n, modulus)
  
  # Ensure result is non-negative
  (inv + modulus) % modulus

# --- Main Program Logic ---

main = ->
  # --- Part 1 Constants (Example/Not used for final run) ---
  # DECK_SIZE_P1 = 10007
  # TARGET_CARD_P1 = 2019

  # --- Part 2 Constants ---
  DECK_SIZE = 119315717514047n # Use BigInt
  SHUFFLE_REPEATS = 101741582076661n # Use BigInt
  TARGET_POSITION = 2020n # Use BigInt

  # Read instructions from input.txt
  try
    instructions = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
  catch error
    console.error "Error reading input.txt:", error
    process.exit(1)

  # --- Part 2 Calculation ---
  # We want to find which card ends up at TARGET_POSITION.
  # This is equivalent to figuring out the initial position of the card
  # that lands at TARGET_POSITION after SHUFFLE_REPEATS shuffles.
  #
  # Let the position of a card be P. Each shuffle technique is a linear
  # transformation: P_new = (a * P_old + b) mod DECK_SIZE.
  # We can find the combined (a, b) for one full shuffle pass.
  #
  # To find the card at a final position, it's easier to work backwards.
  # We find the inverse transformation for each step and compose them.
  # The inverse transformation maps the final position to the initial position.
  # Let the inverse transformation for one shuffle be P_initial = (A * P_final + B) mod DECK_SIZE.

  # Initialize the parameters for the *inverse* transformation (identity)
  # P_initial = (1 * P_final + 0) mod DECK_SIZE
  a = 1n
  b = 0n

  # Process instructions in reverse order to find the combined inverse transformation
  for i in [instructions.length - 1 .. 0] by -1
    line = instructions[i]

    if line == "deal into new stack"
      # Inverse: P_prev = M - 1 - P_curr = (-1 * P_curr + (M - 1)) % M
      # Apply transformation (-1, M-1) to (a, b)
      # new_a = (-1 * a) % M
      # new_b = (-1 * b + (M - 1)) % M
      a = -a
      b = -b + (DECK_SIZE - 1n)
      a = (a + DECK_SIZE) % DECK_SIZE # Ensure positive
      b = (b + DECK_SIZE) % DECK_SIZE # Ensure positive

    else if line.startsWith "cut"
      n = BigInt(parseInt(line.split(" ").pop()))
      # Inverse: P_prev = (P_curr + n) % M = (1 * P_curr + n) % M
      # Apply transformation (1, n) to (a, b)
      # new_a = (1 * a) % M = a
      # new_b = (1 * b + n) % M
      # a remains a
      b = b + n
      b = (b + DECK_SIZE) % DECK_SIZE # Ensure positive

    else if line.startsWith "deal with increment"
      n = BigInt(parseInt(line.split(" ").pop()))
      # Inverse: P_prev = (P_curr * modInverse(n, M)) % M = (modInverse(n, M) * P_curr + 0) % M
      # Apply transformation (modInverse(n, M), 0) to (a, b)
      inv_n = modInverse(n, DECK_SIZE)
      # new_a = (inv_n * a) % M
      # new_b = (inv_n * b + 0) % M
      a = (a * inv_n) % DECK_SIZE
      b = (b * inv_n) % DECK_SIZE
      a = (a + DECK_SIZE) % DECK_SIZE # Ensure positive (though likely already)
      b = (b + DECK_SIZE) % DECK_SIZE # Ensure positive

  # Now (a, b) represents the inverse transformation for ONE shuffle pass:
  # P_initial = (a * P_final + b) % DECK_SIZE
  # We need to apply this transformation SHUFFLE_REPEATS times.
  # Let the transformation be T(P) = a*P + b.
  # T^K(P) = (a^K * P + b * (a^K - 1) * modInverse(a - 1, M)) % M
  # Where K = SHUFFLE_REPEATS, M = DECK_SIZE

  # Calculate a^K mod M
  a_pow_k = modPow(a, SHUFFLE_REPEATS, DECK_SIZE)

  # Calculate (a^K - 1) mod M
  a_pow_k_minus_1 = (a_pow_k - 1n + DECK_SIZE) % DECK_SIZE

  # Calculate modInverse(a - 1, M)
  a_minus_1_inv = modInverse(a - 1n, DECK_SIZE)

  # Calculate the B term for K shuffles: b * (a^K - 1) * (a-1)^(-1)
  b_k = (b * a_pow_k_minus_1 * a_minus_1_inv) % DECK_SIZE
  b_k = (b_k + DECK_SIZE) % DECK_SIZE # Ensure positive

  # The final inverse transformation after K shuffles is:
  # P_initial = (a_pow_k * P_final + b_k) % DECK_SIZE
  # We want the initial position (card number) for P_final = TARGET_POSITION

  initial_pos = (a_pow_k * TARGET_POSITION + b_k) % DECK_SIZE
  initial_pos = (initial_pos + DECK_SIZE) % DECK_SIZE # Final ensure positive

  # The initial position corresponds to the card number
  console.log initial_pos.toString() # Output the final card number as a string

# --- Entry Point ---
# Execute main only if the script is run directly
main() if require.main is module

