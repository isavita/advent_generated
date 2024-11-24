
import std/[md5, strutils, strformat]

proc getMD5Hash(input: string): string =
  return getMD5(input)

proc findTriplet(hash: string): string =
  for i in 0 ..< hash.len - 2:
    if hash[i] == hash[i+1] and hash[i] == hash[i+2]:
      return $hash[i]
  return ""

proc main() =
  let salt = readFile("input.txt").strip()
  var keys = 0
  var index = 0

  while keys < 64:
    let hash = getMD5Hash(salt & $index)
    let triplet = findTriplet(hash)
    
    if triplet != "":
      block checkNext:
        for i in 1..1000:
          let nextHash = getMD5Hash(salt & $((index + i)))
          if nextHash.contains(triplet.repeat(5)):
            keys.inc
            break checkNext
    
    index.inc

  echo index - 1

main()
