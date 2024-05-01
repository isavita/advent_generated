import sequtils, strutils, algorithm

let input = readFile("input.txt").split("\n\n")

var elves: seq[int] = @[]
for elf in input:
  let calories = elf.splitLines().map(parseInt).foldl(a + b, 0)
  elves.add calories

elves.sort(Descending)

echo "Part 1: ", elves[0]
echo "Part 2: ", elves[0..2].foldl(a + b, 0)