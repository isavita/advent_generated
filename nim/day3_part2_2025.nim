
import strutils

proc largestSubseq(s: string, k: int): string =
  var toRemove = s.len - k
  var stack: seq[char] = @[]
  for c in s:
    while toRemove > 0 and stack.len > 0 and stack[^1] < c:
      stack.delete(stack.len - 1)
      dec toRemove
    stack.add(c)
  result = newString(k)
  for i in 0..<k:
    result[i] = stack[i]

proc main() =
  var total: int64 = 0
  try:
    for line in "input.txt".lines:
      if line.strip.len > 0:
        let val = parseBiggestInt(largestSubseq(line.strip, 12))
        total += val
    echo total
  except IOError:
    echo "Error reading input.txt"
  except ValueError:
    echo "Error parsing number"

when isMainModule:
  main()
