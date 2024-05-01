import strutils, sequtils

proc solve(input: string): string =
  let recipesToMake = parseInt(input)
  var scoreboard = @[3, 7]
  var elf1 = 0
  var elf2 = 1
  while scoreboard.len < recipesToMake + 10:
    let sum = scoreboard[elf1] + scoreboard[elf2]
    if sum >= 10:
      scoreboard.add(sum div 10)
    scoreboard.add(sum mod 10)
    elf1 = (elf1 + scoreboard[elf1] + 1) mod scoreboard.len
    elf2 = (elf2 + scoreboard[elf2] + 1) mod scoreboard.len
  result = scoreboard[recipesToMake..recipesToMake+9].mapIt($it).join("")

when isMainModule:
  let input = readFile("input.txt").strip()
  echo solve(input)