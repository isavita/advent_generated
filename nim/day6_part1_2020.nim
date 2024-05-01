import sequtils, strutils, sets

proc countYesQuestions(filename: string): int =
  var totalYes = 0
  var groupYes: HashSet[char]

  for line in lines(filename):
    if line.len == 0:
      totalYes += groupYes.len
      groupYes = initHashSet[char]()
    else:
      for c in line:
        groupYes.incl(c)

  totalYes += groupYes.len
  return totalYes

echo countYesQuestions("input.txt")