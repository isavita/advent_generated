def input = new File('input.txt').text.trim().split('\n')
def cubes = [red: 12, green: 13, blue: 14]
def possibleGames = []

input.eachWithIndex { line, index ->
    def gameId = index + 1
    def subsets = line.substring(line.indexOf(':') + 2).split(';').collect { subset ->
        def counts = subset.trim().split(',').collectEntries { cube ->
            def (count, color) = cube.trim().split()
            [(color): count.toInteger()]
        }
        counts
    }
    def isValid = subsets.every { subset ->
        subset.every { color, count ->
            cubes[color] >= count
        }
    }
    if (isValid) {
        possibleGames << gameId
    }
}

println possibleGames.sum()