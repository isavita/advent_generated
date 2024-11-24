
import std/[strutils, tables, algorithm]

proc getLeastCommonChar(count: CountTable[char]): char =
  result = count.smallest.key

proc getOriginalMessage(messages: seq[string]): string =
  if messages.len == 0:
    return ""

  let messageLength = messages[0].len
  var count = newSeq[CountTable[char]](messageLength)
  for i in 0..<messageLength:
    count[i] = initCountTable[char]()

  for message in messages:
    for j, ch in message:
      count[j].inc(ch)

  result = newStringOfCap(messageLength)
  for charCount in count:
    result.add charCount.getLeastCommonChar()

proc main() =
  let messages = readFile("input.txt").strip.splitLines()
  echo getOriginalMessage(messages)

main()
