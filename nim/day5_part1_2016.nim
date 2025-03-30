
import std/md5
import system # Provides readFile, strip, $, echo, inc
import strutils # Provides startsWith (optional, slice is fine)

proc main() =
  let doorId = readFile("input.txt").strip()
  var password = ""
  var index = 0

  while password.len < 8:
    let hashInput = doorId & $index
    let result = md5.getMD5(hashInput) # getMD5 returns hex digest directly

    # Check if the first 5 characters are '0'
    if result[0..4] == "00000":
      password.add(result[5]) # Append the 6th character

    inc index # Increment the index

  echo password

when isMainModule:
  main()
