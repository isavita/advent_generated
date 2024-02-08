
def inputFile = new File("input.txt")
def lines = inputFile.readLines()
def algorithm = lines[0].replaceAll("\n", "")
def image = lines.subList(2, lines.size()).collect { it.toCharArray() }

def enhancedImage = enhanceImage(image, algorithm, 2)
println countLitPixels(enhancedImage)

def enhanceImage(image, algorithm, times) {
    for (i in 0..<times) {
        image = applyAlgorithm(image, algorithm, (i % 2 == 1) && (algorithm[0] == '#'))
    }
    return image
}

def applyAlgorithm(image, algorithm, flip) {
    def enhancedImage = new char[image.size() + 2][image[0].size() + 2]
    for (i in 0..<enhancedImage.size()) {
        for (j in 0..<enhancedImage[i].size()) {
            def index = calculateIndex(i-1, j-1, image, flip)
            enhancedImage[i][j] = algorithm.charAt(index)
        }
    }
    return enhancedImage
}

def calculateIndex(i, j, image, flip) {
    def index = 0
    for (di in -1..1) {
        for (dj in -1..1) {
            index <<= 1
            if (i+di >= 0 && i+di < image.size() && j+dj >= 0 && j+dj < image[0].size()) {
                if (image[i+di][j+dj] == '#') {
                    index |= 1
                }
            } else if (flip) {
                index |= 1
            }
        }
    }
    return index
}

def countLitPixels(image) {
    def count = 0
    image.each { row ->
        row.each { pixel ->
            if (pixel == '#') {
                count++
            }
        }
    }
    return count
}
