import sequtils, strutils

proc mostFrequentChar(chars: seq[char]): char =
  var charCount: array[256, int]
  for c in chars:
    inc charCount[ord(c)]
  var maxCount = 0
  var maxChar: char
  for c in chars:
    if charCount[ord(c)] > maxCount:
      maxCount = charCount[ord(c)]
      maxChar = c
  maxChar

proc errorCorrectedMessage(filename: string): string =
  let file = readFile(filename)
  let lines = file.splitLines()
  var columns: seq[seq[char]]
  for line in lines:
    if columns.len == 0:
      columns = newSeq[seq[char]](line.len)
    for i, c in line:
      columns[i].add(c)
  for column in columns:
    result.add mostFrequentChar(column)

echo errorCorrectedMessage("input.txt")