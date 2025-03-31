
import std/[strutils, tables, sequtils, options, parseutils, math, algorithm]

# --- Type Definitions ---

type
  GateType* = enum  ## Type of logic gate
    gtAnd, gtOr, gtXor

  Gate* = object    ## Represents a logic gate connection
    op*: GateType
    in1*, in2*, output*: string ## Input and output wire names

# --- Parsing Functions ---

proc parseGateType(s: string): GateType =
  ## Parses a string representation ("AND", "OR", "XOR") into a GateType enum.
  case s.normalize() # Use normalize for case-insensitivity and ignore surrounding whitespace
  of "and": result = gtAnd
  of "or":  result = gtOr
  of "xor": result = gtXor
  else: raise newException(ValueError, "Invalid gate type: " & s)

proc parseGateLine(line: string): Gate =
  ## Parses a line defining a gate (e.g., "x00 AND y00 -> z00")
  let parts = line.split({' '}, maxsplit = 4) # Split by space
  # Expecting: [in1, op_str, in2, "->", output]
  if parts.len == 5 and parts[3] == "->":
    result = Gate(in1: parts[0],
                  op: parseGateType(parts[1]),
                  in2: parts[2],
                  output: parts[4])
  else:
    raise newException(ValueError, "Invalid gate definition line format: " & line)

proc parseValueLine(line: string): (string, int) =
  ## Parses a line defining an initial wire value (e.g., "x00: 1")
  let parts = line.split(": ")
  if parts.len == 2:
    let wireName = parts[0]
    let value = parts[1].parseInt
    if value != 0 and value != 1:
      raise newException(ValueError, "Invalid initial value (must be 0 or 1): " & line)
    result = (wireName, value)
  else:
    raise newException(ValueError, "Invalid initial value line format: " & line)

# --- Evaluation Function ---

proc evaluateGate(op: GateType, v1, v2: int): int =
  ## Calculates the output of a gate given its type and input values.
  assert v1 == 0 or v1 == 1
  assert v2 == 0 or v2 == 1
  case op:
  of gtAnd: result = v1 and v2
  of gtOr:  result = v1 or v2
  of gtXor: result = v1 xor v2

# --- Main Simulation Logic ---

proc main() =
  ## Reads input, simulates the circuit, and prints the final result.
  var
    wireValues = initTable[string, Option[int]]() # Stores the computed value (0/1) or none if not yet computed
    allGates = newSeq[Gate]()                     # List of all gates defined in the input
    pendingGates = newSeq[Gate]()                 # Gates whose output is not yet computed
    readingValues = true                          # State flag for parsing input sections

  # 1. Read and Parse Input
  try:
    for line in "input.txt".lines:
      let strippedLine = line.strip()
      if strippedLine.len == 0:
        if readingValues: # Transition from values section to gates section
           readingValues = false
        continue # Skip empty lines

      if readingValues:
        let (wireName, value) = parseValueLine(strippedLine)
        wireValues[wireName] = some(value)
      else:
        let gate = parseGateLine(strippedLine)
        allGates.add(gate)
        # Optimization: Immediately check if the output wire exists from initial values.
        # If so, no need to add this gate to pending (though unlikely scenario based on problem description)
        if not wireValues.contains(gate.output) or wireValues[gate.output].isNone:
           pendingGates.add(gate)

  except IOError:
    echo "Error: Could not read input file 'input.txt'"
    quit(1)
  except ValueError as e:
    echo "Error parsing input: ", e.msg
    quit(1)

  # 2. Simulate the Circuit
  var madeProgress: bool
  while pendingGates.len > 0:
    madeProgress = false
    var nextPendingGates = newSeq[Gate]() # Gates remaining for the *next* iteration

    for gate in pendingGates:
      # Check if output wire already has a value (should only happen if it was an initial value)
      # We already handled this partially during parsing, but double-check.
      if wireValues.contains(gate.output) and wireValues[gate.output].isSome:
        # This gate's output was somehow already determined (likely initial input).
        # It shouldn't be in pending, but let's handle it gracefully.
        madeProgress = true # Count as progress as it removes a pending gate
        continue

      # Get optional values for inputs
      let val1Opt = wireValues.getOrDefault(gate.in1)
      let val2Opt = wireValues.getOrDefault(gate.in2)

      # Check if both inputs are available
      if val1Opt.isSome and val2Opt.isSome:
        # Evaluate the gate
        let result = evaluateGate(gate.op, val1Opt.get, val2Opt.get)
        # Store the result for the output wire
        wireValues[gate.output] = some(result)
        # Mark that progress was made in this iteration
        madeProgress = true
        # This gate is now processed, do not add it to nextPendingGates
      else:
        # One or both inputs not yet available, keep this gate for the next iteration
        nextPendingGates.add(gate)

    # Check for stalls (no progress made but gates still pending)
    # This indicates an issue like unconnected inputs or a cycle (problem guarantees no cycles)
    if not madeProgress and nextPendingGates.len > 0:
       echo "Error: Simulation stalled. Cannot resolve values for remaining gates:"
       for gate in nextPendingGates:
         echo "  Gate: ", gate.in1, " ", gate.op, " ", gate.in2, " -> ", gate.output
         echo "    Input ", gate.in1, " value: ", wireValues.getOrDefault(gate.in1)
         echo "    Input ", gate.in2, " value: ", wireValues.getOrDefault(gate.in2)
       quit(1)

    # Update the list of pending gates for the next iteration
    pendingGates = nextPendingGates

  # 3. Calculate Final Output Number
  var zWireBits = newSeq[tuple[index: int, value: int]]()

  for wireName, valueOpt in wireValues.pairs:
    if wireName.startsWith("z"):
      if valueOpt.isSome:
        try:
          # Extract the numerical index after "z"
          let index = parseInt(wireName.substr(1))
          zWireBits.add((index: index, value: valueOpt.get))
        except ValueError:
          echo "Warning: Could not parse index from Z wire name: ", wireName
      else:
        # This should not happen if the simulation completed correctly
        echo "Warning: Z wire '", wireName, "' has no calculated value."

  # Sort the bits by their index (z00 is least significant)
  zWireBits.sort(proc(a, b: auto): int = cmp(a.index, b.index))

  # Combine the bits into the final decimal number
  var finalNumber: BiggestInt = 0 # Use BiggestInt to handle potentially large results
  for bitInfo in zWireBits:
    if bitInfo.value == 1:
      # Set the corresponding bit in the final number
      # (1 shl index) creates a number with only the 'index'-th bit set
      finalNumber = finalNumber or (BiggestInt(1) shl bitInfo.index)
      # Alternative: finalNumber += BiggestInt(bitInfo.value) * (BiggestInt(1) shl bitInfo.index)

  # 4. Print the Result
  echo finalNumber

# --- Program Entry Point ---
when isMainModule:
  main()
