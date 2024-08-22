import strutils

proc hasABBA(s: string): bool =
  for i in 0..<len(s) - 3:
    if s[i] != s[i + 1] and s[i] == s[i + 3] and s[i + 1] == s[i + 2]:
      return true
  return false

proc supportsTLS(ip: string): bool =
  var insideBrackets = false
  var hasABBAOutside = false
  var currentSegment = ""

  for c in ip:
    if c == '[':
      if hasABBA(currentSegment):
        hasABBAOutside = true
      insideBrackets = true
      currentSegment = ""
    elif c == ']':
      if hasABBA(currentSegment):
        return false
      insideBrackets = false
      currentSegment = ""
    else:
      currentSegment.add(c)

  if hasABBA(currentSegment):
    hasABBAOutside = true

  return hasABBAOutside

when isMainModule:
  let file = "input.txt"
  var count = 0
  for line in lines(file):
    if supportsTLS(line):
      inc count
  echo count