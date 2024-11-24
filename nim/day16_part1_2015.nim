
import std/[strutils, parseutils, tables, streams]

proc main() =
  var mfcsam = {
    "children": 3, "cats": 7, "samoyeds": 2, "pomeranians": 3,
    "akitas": 0, "vizslas": 0, "goldfish": 5, "trees": 3,
    "cars": 2, "perfumes": 1
  }.toTable()

  var file = newFileStream("input.txt", fmRead)
  if file == nil:
    quit("Cannot open file")

  for line in file.lines:
    let parts = line.split(" ")
    let sueNumber = parts[1][0..^2]

    block checkSue:
      for i in countup(2, parts.high, 2):
        let item = parts[i][0..^2]
        var count: int
        discard parseInt(parts[i+1][0..^2], count)
        
        if mfcsam.getOrDefault(item) != count:
          break checkSue

      echo sueNumber
      break

  file.close()

main()
