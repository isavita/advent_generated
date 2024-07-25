
import java.nio.file.Files
import java.nio.file.Paths

class CaveSystem {
    Map<String, List<String>> graph = [:]

    void addConnection(String cave1, String cave2) {
        graph.computeIfAbsent(cave1) { [] } << cave2
        graph.computeIfAbsent(cave2) { [] } << cave1
    }

    int countPaths(String currentCave, Set<String> visited) {
        if (currentCave == "end") {
            return 1
        }

        if (visited.contains(currentCave) && currentCave.toLowerCase() == currentCave) {
            return 0
        }

        visited.add(currentCave)
        int totalPaths = 0

        for (String neighbor : graph.getOrDefault(currentCave, [])) {
            totalPaths += countPaths(neighbor, visited)
        }

        visited.remove(currentCave)
        return totalPaths
    }
}

def readInput(String filePath) {
    def caveSystem = new CaveSystem()
    Files.lines(Paths.get(filePath)).forEach { line ->
        def caves = line.split('-')
        caveSystem.addConnection(caves[0], caves[1])
    }
    return caveSystem
}

def main() {
    def caveSystem = readInput("input.txt")
    int numberOfPaths = caveSystem.countPaths("start", new HashSet<>())
    println "Number of distinct paths: $numberOfPaths"
}

main()
