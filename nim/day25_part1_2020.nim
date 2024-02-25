
import os
import strutils

proc transform(subjectNumber, loopSize: int): int =
  var value = 1
  for i in 0 ..< loopSize:
    value *= subjectNumber
    value = value mod 20201227
  result = value

proc findLoopSize(publicKey: int): int =
  var value = 1
  var loopSize = 0
  while value != publicKey:
    value *= 7
    value = value mod 20201227
    inc loopSize
  result = loopSize

var file = open("input.txt")
var cardPublicKey = parseInt(file.readLine())
var doorPublicKey = parseInt(file.readLine())

var cardLoopSize = findLoopSize(cardPublicKey)
var encryptionKey = transform(doorPublicKey, cardLoopSize)

echo encryptionKey
