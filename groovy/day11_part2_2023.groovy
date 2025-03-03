
import java.nio.file.Files
import java.nio.file.Paths

def solvePart1(image) {
    def expandedImage = expandUniverse(image, 2)
    def galaxies = findGalaxies(expandedImage)
    def sumOfShortestPaths = calculateSumOfShortestPaths(galaxies)
    return sumOfShortestPaths
}

def solvePart2(image, expansionFactor) {
    def expandedImage = expandUniverse(image, expansionFactor)
    def galaxies = findGalaxies(expandedImage)
    def sumOfShortestPaths = calculateSumOfShortestPaths(galaxies)
    return sumOfShortestPaths
}

def expandUniverse(image, expansionFactor) {
    def rowsToExpand = []
    for (int i = 0; i < image.size(); i++) {
        if (!image[i].contains('#')) {
            rowsToExpand << i
        }
    }

    def colsToExpand = []
    for (int j = 0; j < image[0].size(); j++) {
        def column = []
        for (int i = 0; i < image.size(); i++) {
            column << image[i][j]
        }
        if (!column.contains('#')) {
            colsToExpand << j
        }
    }

    def expandedGalaxies = []
    image.eachWithIndex { row, rowIndex ->
        def expandedRowIndex = rowIndex + rowsToExpand.findAll { it < rowIndex }.size() * (expansionFactor - 1)
        row.eachWithIndex { cell, colIndex ->
            if (cell == '#') {
                def expandedColIndex = colIndex + colsToExpand.findAll { it < colIndex }.size() * (expansionFactor - 1)
                expandedGalaxies << [expandedRowIndex, expandedColIndex]
            }
        }
    }

    return expandedGalaxies
}

def findGalaxies(expandedImage) {
    return expandedImage
}

def calculateSumOfShortestPaths(galaxies) {
    long sum = 0
    for (int i = 0; i < galaxies.size(); i++) {
        for (int j = i + 1; j < galaxies.size(); j++) {
            def galaxy1 = galaxies[i]
            def galaxy2 = galaxies[j]
            sum += Math.abs(galaxy1[0] - galaxy2[0]) + Math.abs(galaxy1[1] - galaxy2[1])
        }
    }
    return sum
}

static void main(String[] args) {
    def image = Files.readAllLines(Paths.get("input.txt"))

    def part1Result = solvePart1(image.collect { it.toList() })
    println "Part 1: ${part1Result}"

    def part2Result = solvePart2(image.collect { it.toList() }, 1000000)
    println "Part 2: ${part2Result}"
}
