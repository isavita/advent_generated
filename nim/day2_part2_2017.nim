import strutils, sequtils

proc part1(input: seq[seq[int]]): int =
  for row in input:
    let minVal = min(row)
    let maxVal = max(row)
    result += maxVal - minVal

proc part2(input: seq[seq[int]]): int =
  for row in input:
    for i in 0..<row.len:
      for j in i+1..<row.len:
        if row[i] mod row[j] == 0:
          result += row[i] div row[j]
        elif row[j] mod row[i] == 0:
          result += row[j] div row[i]

when isMainModule:
  let input = "input.txt".lines.toSeq.mapIt(it.splitWhitespace.mapIt(it.parseInt))
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)