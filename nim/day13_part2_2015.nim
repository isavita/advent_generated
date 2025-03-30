
import strutils, tables, sets, algorithm, math

proc main() =
  var happiness = initTable[tuple[p1: string, p2: string], int]()
  var guestsSet = initHashSet[string]()

  for line in lines("input.txt"):
    let parts = line.strip().split()
    if parts.len < 11: continue # Skip potentially malformed lines

    let person1 = parts[0]
    let person2 = parts[^1][0 .. ^2] # Remove trailing '.'
    let amount = parseInt(parts[3])
    let change = if parts[2] == "gain": amount else: -amount

    happiness[(person1, person2)] = change
    guestsSet.incl(person1)
    guestsSet.incl(person2)

  var guestsList = newSeq[string]()
  for guest in guestsSet:
    guestsList.add(guest)

  # Add "You"
  guestsList.add("You")
  guestsList.sort(cmp[string])

  var maxHappiness = low(int)

  while true:
    var currentHappiness = 0
    let n = guestsList.len
    if n == 0: break # Handle empty guest list edge case

    for i in 0 ..< n:
      let p1 = guestsList[i]
      let p2 = guestsList[(i + 1) mod n] # Wrap around for circular table
      currentHappiness += happiness.getOrDefault((p1, p2))
      currentHappiness += happiness.getOrDefault((p2, p1))

    maxHappiness = max(maxHappiness, currentHappiness)

    if not nextPermutation(guestsList):
      break

  echo maxHappiness

main()
