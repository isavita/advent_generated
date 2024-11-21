
def readInput(filename) {
    def lines = new File(filename).readLines()
    def algorithm = lines[0]
    def image = lines.drop(2).collect { it.collect { it == '#' } }
    [algorithm, image]
}

def enhanceImage(algorithm, image, useInfiniteLit) {
    def sizeY = image.size()
    def sizeX = image[0].size()
    def newImage = (0..<sizeY+2).collect { (0..<sizeX+2).collect { false } }

    (0..<sizeY+2).each { y ->
        (0..<sizeX+2).each { x ->
            def index = 0
            (-1..1).each { dy ->
                (-1..1).each { dx ->
                    index <<= 1
                    def ny = y + dy - 1
                    def nx = x + dx - 1
                    if (ny >= 0 && ny < sizeY && nx >= 0 && nx < sizeX) {
                        index |= image[ny][nx] ? 1 : 0
                    } else if (useInfiniteLit) {
                        index |= 1
                    }
                }
            }
            newImage[y][x] = algorithm[index] == '#'
        }
    }
    newImage
}

def countLitPixels(image) {
    image.sum { row -> row.count { it } }
}

def (algorithm, image) = readInput('input.txt')
(0..<50).each { i ->
    image = enhanceImage(algorithm, image, i % 2 == 1 && algorithm[0] == '#')
}
println countLitPixels(image)
