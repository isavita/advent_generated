import strutils

type AuntSue = object
  number: int
  children: int
  cats: int
  samoyeds: int
  pomeranians: int
  akitas: int
  vizslas: int
  goldfish: int
  trees: int
  cars: int
  perfumes: int

proc parseAuntSue(line: string): AuntSue =
  let parts = line.split(": ", 1)
  let number = parseInt(parts[0].split(" ")[1])
  let attributes = parts[1].split(", ")
  var sue = AuntSue(number: number, children: -1, cats: -1, samoyeds: -1, pomeranians: -1, akitas: -1, vizslas: -1, goldfish: -1, trees: -1, cars: -1, perfumes: -1)
  for attr in attributes:
    let attrParts = attr.split(": ")
    let value = parseInt(attrParts[1])
    case attrParts[0]
    of "children": sue.children = value
    of "cats": sue.cats = value
    of "samoyeds": sue.samoyeds = value
    of "pomeranians": sue.pomeranians = value
    of "akitas": sue.akitas = value
    of "vizslas": sue.vizslas = value
    of "goldfish": sue.goldfish = value
    of "trees": sue.trees = value
    of "cars": sue.cars = value
    of "perfumes": sue.perfumes = value
    else: discard
  return sue

proc main() =
  let input = readFile("input.txt").splitLines()
  var aunts: seq[AuntSue] = @[]
  for line in input:
    aunts.add(parseAuntSue(line))

  let target = AuntSue(children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1)

  for aunt in aunts:
    if (aunt.children == -1 or aunt.children == target.children) and
       (aunt.cats == -1 or aunt.cats > target.cats) and
       (aunt.samoyeds == -1 or aunt.samoyeds == target.samoyeds) and
       (aunt.pomeranians == -1 or aunt.pomeranians < target.pomeranians) and
       (aunt.akitas == -1 or aunt.akitas == target.akitas) and
       (aunt.vizslas == -1 or aunt.vizslas == target.vizslas) and
       (aunt.goldfish == -1 or aunt.goldfish < target.goldfish) and
       (aunt.trees == -1 or aunt.trees > target.trees) and
       (aunt.cars == -1 or aunt.cars == target.cars) and
       (aunt.perfumes == -1 or aunt.perfumes == target.perfumes):
      echo "The real Aunt Sue is number: ", aunt.number
      break

when isMainModule:
  main()