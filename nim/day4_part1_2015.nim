import md5
import strutils

let inputFile = "input.txt"
let secretKey = readFile(inputFile).strip()

var i = 1
while true:
  let inputString = secretKey & $i
  let md5Hash = getMD5(inputString)
  if md5Hash.startsWith("00000"):
    echo i
    break
  inc i