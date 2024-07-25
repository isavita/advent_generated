
import java.nio.file.Files
import java.nio.file.Paths

class BridgeBuilder {
    List<List<Integer>> components = []

    BridgeBuilder(String filename) {
        loadComponents(filename)
    }

    void loadComponents(String filename) {
        Files.lines(Paths.get(filename)).forEach { line ->
            def parts = line.split('/')
            components << [parts[0] as Integer, parts[1] as Integer]
        }
    }

    int calculateStrength(List<List<Integer>> bridge) {
        return bridge.sum { it[0] + it[1] }
    }

    void findBridges(int port, List<List<Integer>> currentBridge, List<List<List<Integer>>> allBridges) {
        for (int i = 0; i < components.size(); i++) {
            def component = components[i]
            if (component.contains(port)) {
                // Create a new bridge including this component
                def newBridge = new ArrayList<>(currentBridge)
                newBridge << component
                // Mark this component as used
                components.remove(i)
                // Find the next port to connect
                int nextPort = component[0] == port ? component[1] : component[0]
                findBridges(nextPort, newBridge, allBridges)
                // Backtrack: add the component back
                components.add(i, component)
            }
        }
        if (currentBridge) {
            allBridges << currentBridge
        }
    }

    def findStrongestBridge() {
        List<List<List<Integer>>> allBridges = []
        findBridges(0, [], allBridges)
        return allBridges.collect { calculateStrength(it) }.max()
    }

    def findLongestStrongestBridge() {
        List<List<List<Integer>>> allBridges = []
        findBridges(0, [], allBridges)
        def longestBridges = allBridges.groupBy { it.size() }
        def longestLength = longestBridges.keySet().max()
        return longestBridges[longestLength].collect { calculateStrength(it) }.max()
    }
}

def builder = new BridgeBuilder('input.txt')
println "Strength of the strongest bridge: ${builder.findStrongestBridge()}"
println "Strength of the longest bridge: ${builder.findLongestStrongestBridge()}"
