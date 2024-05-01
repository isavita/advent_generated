import strutils, sequtils, algorithm

proc hasDuplicates(s: seq[string]): bool =
  let sortedSeq = s.mapIt(it.sorted())
  for i in 0..<s.len-1:
    for j in i+1..<s.len:
      if sortedSeq[i] == sortedSeq[j]:
        return true
  return false

proc hasAnagram(s: seq[string]): bool =
  let sortedSeq = s.mapIt(it.sorted())
  for i in 0..<s.len-1:
    for j in i+1..<s.len:
      if sortedSeq[i] == sortedSeq[j]:
        return true
  return false

proc isValidPassphrase(s: seq[string]): bool =
  let noDuplicates = s.len == s.deduplicate().len
  let noAnagrams = not hasAnagram(s)
  return noDuplicates and noAnagrams

proc countValidPassphrases(filename: string): int =
  let file = readFile(filename)
  let passphrases = file.splitLines().mapIt(it.split())
  var count = 0
  for passphrase in passphrases:
    if isValidPassphrase(passphrase):
      count += 1
  return count

echo countValidPassphrases("input.txt")