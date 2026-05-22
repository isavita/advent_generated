
import "io" for File

var target = Num.fromString(File.read("input.txt").trim())
var sideLength = (target.sqrt + 0.5).floor
if (sideLength % 2 == 0) sideLength = sideLength + 1

var maxValue = sideLength * sideLength
var stepsFromEdge = ((sideLength - 1) / 2).floor
var distanceToMiddle = maxValue

for (i in 0..3) {
    var middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i
    var distance = (target - middlePoint).abs
    if (distance < distanceToMiddle) distanceToMiddle = distance
}

System.print(stepsFromEdge + distanceToMiddle)
