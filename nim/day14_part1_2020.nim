
import std/strutils
import std/tables
import std/os
import std/math # Not strictly needed for bitwise, but good practice

# --- Constants ---
const InputFileName = "input.txt"
const MemAddressPrefix = "mem["
const MemAddressSuffix = "]"
const MaskCommand = "mask"
const AssignmentOp = " = "
const BitMaskLength = 36

# --- Procedures ---

proc applyMask(value: uint64, mask0: uint64, mask1: uint64): uint64 =
  ## Applies the bitmask to the given value.
  ## mask0: AND mask (forces bits to 0)
  ## mask1: OR mask (forces bits to 1)
  result = (value or mask1) and mask0

proc parseMask(maskStr: string): (uint64, uint64) =
  ## Parses the mask string into an AND mask (mask0) and an OR mask (mask1).
  ## Returns (mask0, mask1).
  ## mask0 has 0s where the mask string has '0', and 1s elsewhere.
  ## mask1 has 1s where the mask string has '1', and 0s elsewhere.
  if maskStr.len != BitMaskLength:
    raise newException(ValueError, "Invalid mask length: " & $maskStr.len & ", expected " & $BitMaskLength)

  var mask0: uint64 = not 0'u64 # Start with all 1s (will clear bits for '0')
  var mask1: uint64 = 0'u64    # Start with all 0s (will set bits for '1')

  for i, char in maskStr:
    let bitPos = BitMaskLength - 1 - i # Bit position (0-35) from the right
    case char
    of '0':
      # Clear the corresponding bit in mask0
      mask0 = mask0 and not (1'u64 shl bitPos)
    of '1':
      # Set the corresponding bit in mask1
      mask1 = mask1 or (1'u64 shl bitPos)
    of 'X':
      # Keep mask0 bit as 1 and mask1 bit as 0 for this position
      discard
    else:
      raise newException(ValueError, "Invalid character in mask: " & $char)

  result = (mask0, mask1)

proc solve(): uint64 =
  ## Reads input, simulates the docking program, and returns the sum of memory values.
  var memory: Table[uint64, uint64] = initTable[uint64, uint64]()
  var currentMask0: uint64 = not 0'u64 # Default AND mask (all 1s)
  var currentMask1: uint64 = 0'u64    # Default OR mask (all 0s)

  for line in lines(InputFileName):
    let strippedLine = line.strip()
    if strippedLine.len == 0: # Skip empty lines
      continue

    let parts = strippedLine.split(AssignmentOp)
    if parts.len != 2:
      raise newException(ValueError, "Malformed instruction line: " & strippedLine)

    let command = parts[0].strip()
    let valueStr = parts[1].strip()

    if command == MaskCommand:
      # Update the current mask
      (currentMask0, currentMask1) = parseMask(valueStr)
    elif command.startsWith(MemAddressPrefix) and command.endsWith(MemAddressSuffix):
      # Process memory write instruction
      try:
        let addrStr = command[MemAddressPrefix.len ..< ^MemAddressSuffix.len]
        let address = parseUint(addrStr)
        let value = parseUint(valueStr)

        # Apply the current mask to the value
        let maskedValue = applyMask(value, currentMask0, currentMask1)

        # Store the masked value in memory
        memory[address] = maskedValue
      except ValueError as e:
        raise newException(ValueError, "Failed to parse mem instruction '" & strippedLine & "': " & e.msg)
    else:
      raise newException(ValueError, "Unknown command: " & command)

  # Calculate the sum of all values left in memory
  var totalSum: uint64 = 0
  for val in memory.values():
    totalSum += val

  result = totalSum

# --- Main Entry Point ---
when isMainModule:
  # Ensure input file exists
  if not fileExists(InputFileName):
    echo "Error: Input file '" & InputFileName & "' not found."
  else:
    try:
      let finalSum = solve()
      echo finalSum
    except ValueError as e:
      echo "An error occurred during processing: ", e.msg
    except IOError as e:
      echo "An error occurred during file reading: ", e.msg
    except Exception as e:
      echo "An unexpected error occurred: ", e.msg

