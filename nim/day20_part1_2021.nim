
import std/[strutils, sequtils, sugar, algorithm]

proc readInput(filename: string): (string, seq[seq[char]]) =
  let lines = filename.readFile().splitLines()
  let algorithm = lines[0].replace("\n", "")
  let image = lines[2..^1].mapIt(toSeq(it.items))
  return (algorithm, image)

proc calculateIndex(i, j: int, image: seq[seq[char]], flip: bool): int =
  var index = 0
  for di in -1..1:
    for dj in -1..1:
      index = index shl 1
      let ni = i + di
      let nj = j + dj
      if ni >= 0 and ni < image.len and nj >= 0 and nj < image[0].len:
        if image[ni][nj] == '#':
          index = index or 1
      elif flip:
        index = index or 1
  return index

proc applyAlgorithm(image: seq[seq[char]], algorithm: string, flip: bool): seq[seq[char]] =
  result = newSeqWith(image.len + 2, newSeq[char](image[0].len + 2))
  for i in 0..<result.len:
    for j in 0..<result[0].len:
      let index = calculateIndex(i-1, j-1, image, flip)
      result[i][j] = algorithm[index]

proc enhanceImage(image: seq[seq[char]], algorithm: string, times: int): seq[seq[char]] =
  result = image
  for i in 0..<times:
    result = applyAlgorithm(result, algorithm, i mod 2 == 1 and algorithm[0] == '#')

proc countLitPixels(image: seq[seq[char]]): int =
  for row in image:
    result += row.countIt(it == '#')

proc main() =
  let (algorithm, image) = readInput("input.txt")
  let enhancedImage = enhanceImage(image, algorithm, 2)
  echo countLitPixels(enhancedImage)

main()
