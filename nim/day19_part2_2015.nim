import strutils, os

proc main() =
  let content = readFile("input.txt")
  var lines = content.splitLines()
  var idx = 0
  while idx < lines.len and lines[idx].len != 0:
    inc idx
  if idx + 1 >= lines.len:
    quit(1)
  let molecule = lines[idx + 1].strip()
  var total = 0
  var rn = 0
  var ar = 0
  var y = 0
  var i = 0
  while i < molecule.len:
    inc total
    var elem = $molecule[i]
    var len = 1
    if i + 1 < molecule.len and molecule[i + 1].isLowerAscii:
      elem.add molecule[i + 1]
      len = 2
    if elem == "Rn":
      inc rn
    elif elem == "Ar":
      inc ar
    elif elem == "Y":
      inc y
    i += len
  let steps = total - rn - ar - 2 * y - 1
  echo steps

when isMainModule:
  main()
