
import sets, strutils, system

proc main() =
  var programs = initHashSet[string]()
  var heldBy = initHashSet[string]()

  for line in readFile("input.txt").splitLines():
    if line.len == 0: continue

    let parts = line.splitWhitespace()
    let programName = parts[0]
    programs.incl(programName)

    if line.contains(" -> "):
      let arrowPos = line.find(" -> ")
      if arrowPos > 0:
        let heldPart = line[arrowPos + 4 .. ^1]
        for heldProgram in heldPart.split(", "):
          heldBy.incl(heldProgram.strip())

  let bottomSet = programs - heldBy

  # Expecting only one element in the result set
  for bottomProgram in bottomSet:
    echo bottomProgram
    break # Exit after printing the first (and only) element

main()
