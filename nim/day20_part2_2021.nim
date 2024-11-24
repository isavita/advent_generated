
import std/[strutils, sequtils, bitops]

const
  iterations = 50
  expandBy = 1

proc readInput(filename: string): (string, seq[seq[bool]]) =
  let lines = filename.readFile.splitLines
  let algorithm = lines[0]
  
  var image = newSeq[seq[bool]]()
  for line in lines[2..^1]:
    if line.len > 0:
      image.add line.mapIt(it == '#')
  
  return (algorithm, image)

proc enhanceImage(algorithm: string, image: seq[seq[bool]], useInfiniteLit: bool): seq[seq[bool]] =
  let height = image.len + expandBy * 2
  let width = image[0].len + expandBy * 2
  
  result = newSeqWith(height, newSeq[bool](width))
  
  for y in -expandBy..<image.len + expandBy:
    for x in -expandBy..<image[0].len + expandBy:
      var index = 0
      for dy in -1..1:
        for dx in -1..1:
          index = index shl 1
          let ny = y + dy
          let nx = x + dx
          
          if ny >= 0 and ny < image.len and nx >= 0 and nx < image[0].len:
            if image[ny][nx]:
              index = index or 1
          elif useInfiniteLit:
            index = index or 1
      
      result[y + expandBy][x + expandBy] = algorithm[index] == '#'

proc countLitPixels(image: seq[seq[bool]]): int =
  for row in image:
    for pixel in row:
      if pixel:
        inc result

proc main() =
  let (algorithm, initialImage) = readInput("input.txt")
  
  var image = initialImage
  for i in 0..<iterations:
    image = enhanceImage(algorithm, image, i mod 2 == 1 and algorithm[0] == '#')
  
  echo countLitPixels(image)

main()
