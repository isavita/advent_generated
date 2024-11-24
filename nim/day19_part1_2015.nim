
import std/[strutils, sets, os]

proc main() =
  let content = readFile("input.txt").strip().splitLines()
  var
    replacements: seq[array[2, string]]
    molecule: string

  for line in content:
    if line.contains(" => "):
      let parts = line.split(" => ")
      replacements.add([parts[0], parts[1]])
    elif line.len > 0:
      molecule = line

  var molecules = initHashSet[string]()
  for rep in replacements:
    for i in 0..<molecule.len:
      if molecule[i..^1].startsWith(rep[0]):
        let newMolecule = molecule[0..<i] & rep[1] & molecule[i+rep[0].len..^1]
        molecules.incl(newMolecule)

  echo molecules.len

main()
