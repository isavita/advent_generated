import strutils, sequtils

proc hashAlgorithm(s: string): int =
  var currentValue = 0
  for c in s:
    let asciiCode = ord(c)
    currentValue += asciiCode
    currentValue = (currentValue * 17) mod 256
  currentValue

let input = readFile("input.txt").strip()
let steps = input.split(',')
let results = steps.map(hashAlgorithm)
let sumOfResults = results.foldl(a + b, 0)

echo "The sum of the results is: ", sumOfResults