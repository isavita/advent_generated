
import std/[md5, strutils, os]

proc main() =
  let secretKey = readFile("input.txt").strip()
  var number = 0
  
  while true:
    let hash = getMD5(secretKey & $number)
    if hash.startsWith("000000"):
      echo number
      break
    inc number

main()
