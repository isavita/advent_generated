
import std/strutils
import std/sequtils
import std/os
import std/math # For int64

const InputFileName = "input.txt"

# Represents free space on the disk
const FreeSpaceMarker = -1

proc main() =
  # 1. Read input from the file
  let inputLine = readFile(InputFileName).strip()
  if inputLine.len == 0:
    echo "Error: Input file is empty or not found."
    quit(1)

  # 2. Parse input and build the initial disk state
  var disk = newSeq[int]()
  var fileIdCounter = 0
  var isFileLength = true # Start with file length

  for charDigit in inputLine:
    if not charDigit.isDigit():
      echo "Error: Invalid character in input: ", charDigit
      quit(1)

    let length = parseInt($charDigit) # Convert char '0'..'9' to int 0..9

    if isFileLength:
      # Add file blocks
      if length > 0: # Only add if length > 0
        for _ in 1..length:
          disk.add(fileIdCounter)
        inc fileIdCounter
    else:
      # Add free space blocks
      if length > 0: # Only add if length > 0
        for _ in 1..length:
          disk.add(FreeSpaceMarker)

    # Alternate between file length and free space length
    isFileLength = not isFileLength

  # 3. Compact the disk
  var leftmostFree = 0
  var rightmostFile = disk.len - 1

  while true:
    # Find the next free spot from the left
    while leftmostFree < disk.len and disk[leftmostFree] != FreeSpaceMarker:
      inc leftmostFree

    # Find the next file block from the right
    while rightmostFile >= 0 and disk[rightmostFile] == FreeSpaceMarker:
      dec rightmostFile

    # Check termination condition: pointers crossed or met
    if leftmostFree >= rightmostFile:
      break

    # Perform the move: put the file block into the free space
    disk[leftmostFree] = disk[rightmostFile]
    # Mark the original file block position as free
    disk[rightmostFile] = FreeSpaceMarker

    # Advance pointers to continue searching from the next positions
    # (The inner while loops will handle skipping already correct blocks)
    # We could explicitly inc/dec here too, but the loops cover it.
    # inc leftmostFree
    # dec rightmostFile

  # 4. Calculate the checksum
  var checksum: int64 = 0 # Use int64 for potentially large sums
  for i, blockId in disk:
    if blockId != FreeSpaceMarker:
      checksum += int64(i) * int64(blockId) # Multiply position by file ID

  # 5. Print the output
  echo checksum

# --- Main execution ---
when isMainModule:
  main()
