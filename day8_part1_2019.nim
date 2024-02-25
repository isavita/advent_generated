
import strutils

var input = readFile("input.txt").strip
let width = 25
let height = 6
let layerSize = width * height

var minZeroCount = layerSize + 1
var result = 0

for i in 0 ..< input.len div layerSize:
    let layer = input[i * layerSize ..< (i + 1) * layerSize]
    let zeroCount = layer.count('0')

    if zeroCount < minZeroCount:
        minZeroCount = zeroCount
        result = layer.count('1') * layer.count('2')

echo result
