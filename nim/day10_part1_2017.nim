
import strutils, sequtils

proc readInput(filename: string): seq[int] =
  let fileContent = readFile(filename)
  return fileContent.split(',').mapIt(parseInt(it))

proc knotHash(lengths: seq[int], listSize: int = 256): seq[int] =
  var
    list = toSeq(0 ..< listSize)
    currentPosition = 0
    skipSize = 0

  for length in lengths:
    if length > 0:
      # Reverse the sublist
      for i in 0 ..< length div 2:
        let
          pos1 = (currentPosition + i) mod listSize
          pos2 = (currentPosition + length - 1 - i) mod listSize
        swap(list[pos1], list[pos2])

    # Move the current position forward
    currentPosition = (currentPosition + length + skipSize) mod listSize
    # Increase the skip size
    inc(skipSize)

  return list

proc main() =
  let lengths = readInput("input.txt")
  let hashed = knotHash(lengths)
  echo hashed[0] * hashed[1]

main()
