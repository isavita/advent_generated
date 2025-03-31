
import std/[strutils, sequtils, algorithm]
import os

const InputFile = "input.txt"
const FreeSpaceMarker = -1 # Using -1 to represent free space

# Type definition for storing file information (needed for Part 2)
type FileInfo = object
  id: int
  size: int
  startPos: int # Current starting position on the disk

# --- Parsing ---

proc parseInput(inputStr: string): (seq[int], seq[FileInfo]) =
  ## Parses the input string into the initial disk layout and file info.
  ## Returns a tuple containing:
  ##   - disk: seq[int] representing blocks (file ID or FreeSpaceMarker)
  ##   - files: seq[FileInfo] containing info about each file
  
  var disk = newSeq[int]()
  var files = newSeq[FileInfo]()
  var currentPos = 0
  var fileId = 0
  var isFileLength = true # The input alternates: file length, free space length, ...

  for c in inputStr:
    if not c.isDigit():
      raise newException(ValueError, "Invalid character in input: " & c)
    
    let length = parseInt($c) 
    
    if length == 0: # Length 0 means skip this entry type
      isFileLength = not isFileLength
      continue

    if isFileLength:
      # Record file info
      files.add(FileInfo(id: fileId, size: length, startPos: currentPos))
      # Add file blocks to disk
      for _ in 1..length:
        disk.add(fileId)
      inc fileId
    else:
      # Add free space blocks to disk
      for _ in 1..length:
        disk.add(FreeSpaceMarker)
        
    inc(currentPos, length)
    isFileLength = not isFileLength # Toggle for the next digit

  return (disk, files)

# --- Part 1: Block-by-Block Compaction (Not required for final answer, but useful for context) ---
# Kept for completeness, but commented out from main execution as per request.
#[
proc compactBlocks(disk: var seq[int]) =
  ## Compacts the disk by moving individual file blocks from right to left free spaces.
  var leftFreeIdx = 0
  var rightFileIdx = disk.len - 1

  while leftFreeIdx < rightFileIdx:
    # Find the next free space from the left
    while leftFreeIdx < rightFileIdx and disk[leftFreeIdx] != FreeSpaceMarker:
      inc leftFreeIdx
    
    # Find the next file block from the right
    while leftFreeIdx < rightFileIdx and disk[rightFileIdx] == FreeSpaceMarker:
      dec rightFileIdx

    # If we found a pair to swap
    if leftFreeIdx < rightFileIdx:
      # Move the file block left
      disk[leftFreeIdx] = disk[rightFileIdx]
      # Mark the original position as free
      disk[rightFileIdx] = FreeSpaceMarker
      # Move pointers
      inc leftFreeIdx
      dec rightFileIdx
]#

# --- Part 2: File-by-File Compaction ---

proc findLeftmostFit(disk: seq[int], file: FileInfo): int =
  ## Finds the starting index of the leftmost free space block to the left 
  ## of the file's current position that can fit the file.
  ## Returns -1 if no suitable space is found.
  
  result = -1 # Default to not found
  
  # Search only to the left of the file's current starting position
  for i in 0 ..< file.startPos:
    # Check if this position starts a potential free block large enough
    if disk[i] == FreeSpaceMarker:
      var isLargeEnough = true
      # Check if the next file.size - 1 blocks are also free and within bounds
      for j in 1 ..< file.size:
        let checkIdx = i + j
        if checkIdx >= disk.len or disk[checkIdx] != FreeSpaceMarker:
          isLargeEnough = false
          break # Not enough contiguous space here
      
      if isLargeEnough:
        # Found the leftmost suitable spot
        result = i
        break # Stop searching

proc compactFiles(disk: var seq[int], files: var seq[FileInfo]) =
  ## Compacts the disk by moving whole files into the leftmost available space.
  ## Files are processed in decreasing order of ID.

  # Sort files by ID descending to process highest ID first
  # Although files are generated in increasing ID order, let's be explicit
  files.sort(proc(a, b: FileInfo): int = cmp(b.id, a.id)) 

  for i in 0 ..< files.len:
    let currentFile = files[i] # Get a copy to avoid aliasing issues if we modify files[i] later
    
    let targetPos = findLeftmostFit(disk, currentFile)

    if targetPos != -1: # Found a place to move the file
      # Move the file blocks
      for k in 0 ..< currentFile.size:
        disk[targetPos + k] = currentFile.id
      
      # Clear the original file location
      for k in 0 ..< currentFile.size:
        disk[currentFile.startPos + k] = FreeSpaceMarker
        
      # Update the file's recorded position *in the sequence*
      files[i].startPos = targetPos 
      
      # Note: Although we update files[i].startPos, the `currentFile` copy used
      # in findLeftmostFit retains the position *before* the move, which is correct
      # for finding the search limit (0 ..< currentFile.startPos).

# --- Checksum Calculation ---

proc calculateChecksum(disk: seq[int]): int64 =
  ## Calculates the checksum based on the final disk layout.
  result = 0'i64 # Use 64-bit integer for potentially large checksums
  for i, blockId in disk:
    if blockId != FreeSpaceMarker:
      result += int64(i) * int64(blockId)

# --- Main Execution ---

proc main() =
  if not fileExists(InputFile):
    echo "Error: Input file '", InputFile, "' not found."
    quit(1)

  let inputContent = readFile(InputFile).strip()
  if inputContent.len == 0:
      echo "Error: Input file is empty."
      quit(1)

  # --- Part 2 Logic ---
  # Parse input for Part 2 (gets initial disk and file info)
  var (diskP2, filesP2) = parseInput(inputContent)
  
  # Perform file-by-file compaction
  compactFiles(diskP2, filesP2)
  
  # Calculate and print the checksum for Part 2
  let checksumP2 = calculateChecksum(diskP2)
  echo checksumP2

# Program Entry Point
when isMainModule:
  main()
