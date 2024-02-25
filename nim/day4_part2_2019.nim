
import os
import strutils

proc isValidPassword(password: int): bool =
  var s = $password
  var hasDouble = false

  for i in 0..<len(s)-1:
    if s[i] > s[i+1]:
      return false
    if s[i] == s[i+1]:
      if (i == 0 or s[i] != s[i-1]) and (i+2 >= len(s) or s[i] != s[i+2]):
        hasDouble = true

  return hasDouble

var file = open("input.txt")
let rangeStr = file.readLine().strip()
let ranges = split(rangeStr, "-")
let start = parseInt(ranges[0])
let stop = parseInt(ranges[1])

var count = 0
for i in start..stop:
  if isValidPassword(i):
    count.inc

echo count
