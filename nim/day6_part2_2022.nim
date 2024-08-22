import os
import strutils
import sequtils

# Helper function to check if all characters in a sequence are unique
proc allUnique(s: seq[char]): bool =
  for i in 0..<s.len:
    for j in i+1..<s.len:
      if s[i] == s[j]:
        return false
  return true

# Main function to find the start-of-message marker
proc findStartOfMessageMarker(filename: string, markerLength: int): int =
  let file = readFile(filename)
  let data = file.strip()  # Strip any trailing newline or spaces
  let dataSeq = toSeq(data)

  for i in markerLength..<dataSeq.len:
    let window = dataSeq[i-markerLength..i-1]
    if allUnique(window):
      return i

  return -1  # Return -1 if no marker is found

# Main program execution
let markerLength = 14
let result = findStartOfMessageMarker("input.txt", markerLength)
echo "First start-of-message marker after character: ", result