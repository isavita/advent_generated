
import java.nio.file.Files
import java.nio.file.Paths

class CubeConundrum {
    static void main(String[] args) {
        def inputFile = 'input.txt'
        def games = readInput(inputFile)

        // Part 1: Determine which games are possible with given cube counts
        def possibleGames = games.findAll { game ->
            game.subsets.every { subset ->
                subset.red <= 12 && subset.green <= 13 && subset.blue <= 14
            }
        }
        def sumOfPossibleGameIds = possibleGames.collect { it.id }.sum()
        println "Sum of IDs of possible games: $sumOfPossibleGameIds"

        // Part 2: Calculate the minimum required cubes and their power
        def totalPower = games.collect { game ->
            def minCubes = game.subsets.inject([red: 0, green: 0, blue: 0]) { acc, subset ->
                acc.red = Math.max(acc.red, subset.red)
                acc.green = Math.max(acc.green, subset.green)
                acc.blue = Math.max(acc.blue, subset.blue)
                acc
            }
            return minCubes.red * minCubes.green * minCubes.blue
        }.sum()
        println "Sum of the powers of the minimum sets: $totalPower"
    }

    static List<Game> readInput(String filename) {
        def lines = Files.readAllLines(Paths.get(filename))
        return lines.collect { line ->
            def parts = line.split(': ')
            def id = parts[0].replaceAll("\\D", "").toInteger()
            def subsets = parts[1].split(';').collect { subset ->
                def counts = subset.split(',').collectEntries { entry ->
                    def (count, color) = entry.trim().split(' ')
                    [(color): count.toInteger()]
                }
                return new Subset(counts.red ?: 0, counts.green ?: 0, counts.blue ?: 0)
            }
            return new Game(id, subsets)
        }
    }
}

class Game {
    int id
    List<Subset> subsets

    Game(int id, List<Subset> subsets) {
        this.id = id
        this.subsets = subsets
    }
}

class Subset {
    int red
    int green
    int blue

    Subset(int red, int green, int blue) {
        this.red = red
        this.green = green
        this.blue = blue
    }
}
