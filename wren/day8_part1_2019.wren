
import "io" for File

var width = 25
var height = 6
var layerSize = width * height

var imageData = File.read("input.txt").trim()

var minZeros = layerSize + 1
var result = 0

var zeroCount = 0
var oneCount = 0
var twoCount = 0

for (i in 0...imageData.count / layerSize) {
    zeroCount = 0
    oneCount = 0
    twoCount = 0
    
    var start = i * layerSize
    var end = (i + 1) * layerSize
    var layer = imageData[start...end]
    
    for (pixel in layer) {
        if (pixel == "0") {
            zeroCount = zeroCount + 1
        } else if (pixel == "1") {
            oneCount = oneCount + 1
        } else if (pixel == "2") {
            twoCount = twoCount + 1
        }
    }
    
    if (zeroCount < minZeros) {
        minZeros = zeroCount
        result = oneCount * twoCount
    }
}

System.print(result)
