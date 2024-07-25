
def decodeImage(filePath, width, height) {
    // Read the input from the specified file
    def input = new File(filePath).text.trim()
    
    // Calculate the number of layers
    int layerSize = width * height
    int numLayers = input.length() / layerSize
    
    // Initialize layers
    def layers = []
    for (int i = 0; i < numLayers; i++) {
        layers << input[i * layerSize..<(i + 1) * layerSize]
    }
    
    // Part 1: Find the layer with the fewest 0 digits
    def minLayer = layers.min { layer -> layer.count('0') }
    def count1 = minLayer.count('1')
    def count2 = minLayer.count('2')
    def part1Result = count1 * count2
    println "Part 1 Result: $part1Result"

    // Part 2: Decode the image
    def finalImage = []
    for (int i = 0; i < layerSize; i++) {
        finalImage[i] = '2' // Start with transparent pixels
        for (def layer : layers) {
            if (finalImage[i] == '2') {
                finalImage[i] = layer[i] // Take the topmost non-transparent pixel
            }
        }
    }

    // Render the final image
    println "Part 2 Result:"
    for (int i = 0; i < height; i++) {
        def row = finalImage[i * width..<(i + 1) * width].collect { it == '0' ? ' ' : '#' }.join('')
        println row
    }
}

// Specify the dimensions of the image
def width = 25
def height = 6

// Call the decodeImage function with the input file path
decodeImage('input.txt', width, height)
