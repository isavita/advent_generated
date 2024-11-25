
import "io" for File

class ImageDecoder {
    static decode() {
        var imageData = File.read("input.txt").trim()
        
        var width = 25
        var height = 6
        var layerSize = width * height
        
        var finalImage = List.filled(layerSize, "2")
        
        for (i in 0...imageData.count / layerSize) {
            var layerStart = i * layerSize
            var layerEnd = (i + 1) * layerSize
            var layer = imageData[layerStart...layerEnd.min(imageData.count)]
            
            for (j in 0...layer.count) {
                if (finalImage[j] == "2") {
                    finalImage[j] = layer[j]
                }
            }
        }
        
        System.print("Decoded image:")
        for (i in 0...height) {
            var row = ""
            for (j in 0...width) {
                var pixel = finalImage[i * width + j]
                row = row + (pixel == "0" ? " " : "#")
            }
            System.print(row)
        }
    }
}

ImageDecoder.decode()
