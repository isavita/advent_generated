import os, strutils, sequtils, streams

proc main() =
  let f = open("input.txt")
  defer: f.close()
  for line in f.lines():
    stdout.write(line & "\n")

main()