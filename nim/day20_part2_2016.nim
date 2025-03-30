
import std/[strutils, sequtils, algorithm, math]

when isMainModule:
  var ranges = newSeq[tuple[start, stop: int]]()
  try:
    for line in lines("input.txt"):
      let parts = line.strip.split('-')
      if parts.len == 2:
        ranges.add((parseInt(parts[0]), parseInt(parts[1])))
  except IOError:
    stderr.writeLine "Error reading input.txt"
    quit(1)
  except ValueError:
    stderr.writeLine "Error parsing integer from input.txt"
    quit(1)

  if ranges.len == 0:
    # Match Python's behavior for empty input
    echo 4294967295
    quit(0)

  ranges.sort() # Sorts by the first element (start) by default

  var currentMax: int = 0 # Tracks the highest IP covered by merged blocks so far
  var allowedIPs: int = 0
  const maxIP = 4294967295 # Use int for consistency in calculations

  for r in ranges:
    # If the current range starts after the zone covered so far (+1 because ranges are inclusive)
    if r.start > currentMax + 1:
        # Add the count of IPs in the gap
        allowedIPs += r.start - currentMax - 1
    # Update the maximum covered IP, potentially merging/extending the zone
    currentMax = max(currentMax, r.stop)

  # Add allowed IPs after the last blocked range up to the maximum possible IP
  if currentMax < maxIP:
    allowedIPs += maxIP - currentMax

  echo allowedIPs
